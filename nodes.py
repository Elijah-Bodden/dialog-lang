from shared import *


class BinaryExpression(Expression):
    def __init__(self, left: Expression, operator: str, right: Expression, line: int, col: int, program: str) -> None:
        super().__init__("binary_expression", line, col, program)
        self.left = left
        self.operator = operator
        self.right = right

    def eval(self, env: dict[str, Expression]):
        left_evaluated = self.left.eval(env)
        right_evaluated = self.right.eval(env)
        if self.operator == "as":
            if not isinstance(right_evaluated, TypeExpression):
                raise LanguageError(f"Expected a literal type following the 'as' operator, got {right_evaluated.type} instead", right_evaluated.line, right_evaluated.col, right_evaluated.program)
            return as_type(left_evaluated, right_evaluated.value)
        self.check_compatibility(left_evaluated.type, right_evaluated.type, self.operator)
        try:
            result_value, result_type = BINARY_OPERATIONS[self.operator](
                    left_evaluated, right_evaluated
                )
            return EvaluatedExpression(
                result_value,
                result_type,
                self.line,
                self.col,
                self.program,
            )
        except LanguageError as e:
            # Prevents errors from bubbling up and giving very uninformative messages
            raise e

    def __str__(self):
        return f"[{super().__str__()} {self.left} {self.operator} {self.right}]"

    def __repr__(self):
        return self.__str__()
    
    def pretty_string(self):
        return indent("{\n" + self.left.pretty_string() + " " + self.operator + "\n" + self.right.pretty_string() + "\n}")
    
    def check_compatibility(self, left_type: str, right_type: str, operator: str) -> None:
        if left_type not in BINARY_OPERATOR_TYPE_SUPPORT[operator] or right_type not in BINARY_OPERATOR_TYPE_SUPPORT[operator]:
            raise LanguageError(f"Operator {operator} is not compatible with types {left_type} and {right_type}", self.line, self.col, self.program)
        elif left_type != right_type:
            raise LanguageError(f"Cannot perform mixed-type operation on {left_type} and {right_type}. Cast one of them to the other.", self.line, self.col, self.program)
        


class UnaryExpression(Expression):
    def __init__(self, operator: str, expr: Expression, line: int, col: int, program: str) -> None:
        super().__init__("unary_expression", line, col, program)
        self.operator = operator
        self.expr = expr

    def eval(self, env: dict[str, Expression]):
        expr_evaluated = self.expr.eval(env)
        self.check_compatibility(expr_evaluated.type, self.operator)
        try:
            result_value, result_type = UNARY_OPERATIONS[self.operator](expr_evaluated)
            return EvaluatedExpression(
                result_value,
                result_type,
                self.line,
                self.col,
                self.program,
            )
        except LanguageError as e:
            # Prevents errors from bubbling up and giving very uninformative messages
            raise e

    def __str__(self):
        return f"[{super().__str__()} {self.operator} {self.expr}]"

    def __repr__(self):
        return self.__str__()
    
    def pretty_string(self):
        return indent("{\n" + self.operator + "\n" + self.expr.pretty_string() + "}")
    
    def check_compatibility(self, expr_type: str, operator: str) -> None:
        if expr_type not in UNARY_OPERATOR_TYPE_SUPPORT[operator]:
            raise LanguageError(f"Operator {operator} is not compatible with type {expr_type}", self.line, self.col, self.program)


class LiteralExpression(Expression):
    def __init__(self, literal, type: str, line: int, col: int, program: str) -> None:
        super().__init__(type, line, col, program)
        self.literal = literal

    def eval(self, env: dict[str, Expression]):
        return EvaluatedExpression(self.literal, self.type, self.line, self.col, self.program)

    def __str__(self):
        return f"[{super().__str__()} {self.literal}]"

    def __repr__(self):
        return self.__str__()
    
    def pretty_string(self):
        return indent(self.__str__())


class IdentifierRefrence(Expression):
    def __init__(self, identifier: str, line: int, col: int, program: str) -> None:
        super().__init__("identifier", line, col, program)
        self.identifier = identifier

    def eval(self, env: dict[str, Expression]):
        try:
            # Because expressions are evaluated on assignment
            evaluated_expr = env[self.identifier]
            return EvaluatedExpression(evaluated_expr.value, evaluated_expr.type, self.line, self.col, self.program)
        except KeyError:
            raise LanguageError(
                f"Identifier not found: {self.identifier}",
                self.line,
                self.col,
                self.program,
            )

    def __str__(self):
        return f"[{super().__str__()} {self.identifier}]"

    def __repr__(self):
        return self.__str__()
    
    def pretty_string(self):
        return indent(self.__str__())


class Statement:
    def __init__(self, type: str, line: int, col: int, program: str) -> None:
        self.type = type
        self.line = line
        self.col = col
        self.program = program

    def __str__(self):
        return f"[{self.type} statement]"

    def __repr__(self):
        return self.__str__()

    def get_error(self, message: str):
        return LanguageError(message, self.line, self.col, self.program)

class BlockStatement(Statement):
    def __init__(self, statements: list[Statement], line: int, col: int, program: str) -> None:
        super().__init__("block", line, col, program)
        self.statements = statements

    def eval(self, env):
        try:
            for statement in self.statements:
                statement.eval(env)
        except LanguageError as e:
            # Prevents errors from bubbling up and giving very uninformative messages
            raise e

    def __str__(self):
        return f"BLOCK: {{{self.statements}}}"

    def __repr__(self):
        return self.__str__()
    
    def pretty_string(self):
        string = "{" +  "\n".join([statement.pretty_string() for statement in self.statements]) + "\n}"
        return indent(string)

class Function(Expression):
    def __init__(self, args: list[str], body: BlockStatement, line: int, col: int, program: str) -> None:
        # args is a list of identifier strings
        self.args = args
        self.body = body
        super().__init__("function", line, col, program)
        self.value = self

    def __str__(self):
        return f"[FUNCTION ({self.args}) => {self.body}]"

    def __repr__(self):
        return self.__str__()

    def eval(self, env):
        return self

    def call(self, env, args, call_line: int, call_col: int, call_program: str):
        if len(self.args) != len(args):
            raise LanguageError(
                f"Wrong number of arguments for function {self.args} (expected {len(self.args)}, got {len(args)})",
                call_line,
                call_col,
                call_program,
            )
        scope = env.copy()
        for arg, value in zip(self.args, args):
            scope[arg] = value.eval(scope)
        result = self.body.eval(scope)
        return result if result else EvaluatedExpression("BYPASS", "BYPASS", call_line, call_col, call_program)

    def pretty_string(self):
        return indent("{\nFUNCTION: " + str(self.args) + "=>\n" + self.body.pretty_string() + "\n}")


class FunctionCall(Expression):
    def __init__(self, function: Function, args: list[Expression], identifier: str | None = None, line: int = 0, col: int = 0, program: str = "") -> None:
        super().__init__("function_call", line, col, program)
        self.args = args
        self.function = function
        self.identifier = identifier

    def eval(self, env):
        try:
            evaluated_function = self.function.eval(env).value.call(env, self.args, self.line, self.col, self.program)
            return EvaluatedExpression(
                evaluated_function.value,
                evaluated_function.type,
                self.line,
                self.col,
                self.program,
            )
        except LanguageError as e:
            # Prevents errors from bubbling up and giving very uninformative messages
            raise e

    def __str__(self):
        return f"[FUNCTION_CALL {self.identifier if self.identifier else self.function}{self.args}]"

    def __repr__(self):
        return self.__str__()
    
    def pretty_string(self):
        return indent("{\nFUNCTION CALL\n" + (self.identifier if  self.identifier else self.function.pretty_string()) + str(self.args) + "\n}")

class PrintStatement(Statement):
    def __init__(self, expr: Expression, line: int, col: int, program: str) -> None:
        super().__init__("print", line, col, program)
        self.expr = expr

    def eval(self, env: dict):
        evaluated_expr = as_type(self.expr.eval(env), "string")
        print(evaluated_expr.value)

    def __str__(self):
        return f"PRINT {{{self.expr}}}"

    def __repr__(self):
        return self.__str__()

    def pretty_string(self):
        return indent("{\nPRINT\n" + self.expr.pretty_string() + "\n}")



class WhileStatement(Statement):
    def __init__(
        self,
        condition: Expression,
        block: BlockStatement,
        line: int,
        col: int,
        program: str,
    ) -> None:
        super().__init__("while", line, col, program)
        self.condition = condition
        self.block = block

    def eval(self, env: dict):
        evaluated_condition = self.condition.eval(env)
        if evaluated_condition.type != "boolean":
            raise LanguageError(f"Condition must be a boolean, got {evaluated_condition.type}. Cast it to a boolean with 'as boolean'", self.condition.line, self.condition.col, self.program)
        while evaluated_condition.value:
            self.block.eval(env)
            evaluated_condition = self.condition.eval(env)
            if evaluated_condition.type != "boolean":
                raise LanguageError(f"Condition must be a boolean, got {evaluated_condition.type}. Cast it to a boolean with 'as boolean'", self.condition.line, self.condition.col, self.program)

    def __str__(self):
        return f"WHILE {{{self.condition}}} DO {{{self.block}}}"

    def __repr__(self):
        return self.__str__()

    def pretty_string(self):
        return indent("{\nWHILE\n" + self.condition.pretty_string() + " DO\n" + self.block.pretty_string() + "\n}")


class ForStatement(Statement):
    def __init__(
        self,
        condition: Expression,
        block: BlockStatement,
        modifier_statement: Statement,
        line: int,
        col: int,
        program: str,
    ) -> None:
        super().__init__("for", line, col, program)
        self.condition = condition
        self.modifier_statement = modifier_statement
        self.block = block

    def eval(self, env: dict):
        evaluated_condition = self.condition.eval(env)
        if evaluated_condition.type != "boolean":
            raise LanguageError(f"Condition must be a boolean, got {evaluated_condition.type}. Cast it to a boolean with 'as boolean'", self.condition.line, self.condition.col, self.program)
        while evaluated_condition.value:
            self.block.eval(env)
            self.modifier_statement.eval(env)
            evaluated_condition = self.condition.eval(env)
            if evaluated_condition.type != "boolean":
                raise LanguageError(f"Condition must be a boolean, got {evaluated_condition.type}. Cast it to a boolean with 'as boolean'", self.condition.line, self.condition.col, self.program)

    def __str__(self):
        return f"FOR ({self.condition}) DO {self.block} MODIFIER: {{{self.modifier_statement}}}"

    def __repr__(self):
        return self.__str__()
    
    def pretty_string(self):
        return indent("{\nFOR\n" + self.condition.pretty_string() + " DO\n" + self.block.pretty_string() + " MODIFIER:\n" + self.modifier_statement.pretty_string() + "\n}")


class ErrorStatement(Statement):
    def __init__(self, message: Expression, line: int, col: int, program: str):
        super().__init__("error", line, col, program)
        self.message = message
        self.line = line
        self.col = col
        self.program = program

    def eval(self, env: dict):
        evaluated_message = as_type(self.message.eval(env), "string")
        raise LanguageError(evaluated_message.value, self.line, self.col, self.program)

    def __str__(self):
        return f"ERROR {{{self.message}}}"

    def __repr__(self):
        return self.__str__()
    
    def pretty_string(self):
        return indent("{\nERROR\n" + self.message.pretty_string() + "\n}")


class AssignmentStatement(Statement):
    def __init__(
        self,
        identifier: IdentifierRefrence,
        expr: Expression,
        asssignment_type: str="assign",
        line: int = 0,
        col: int = 0,
        program: str = "",
    ) -> None:
        super().__init__("assignment", line, col, program)
        self.identifier = identifier
        self.expr = expr
        self.assigment_type = asssignment_type

    def eval(self, env: dict):
        if self.assigment_type != "assign":
            if not self.assigment_type in ASSIGNMENT_OPERATIONS:
                raise LanguageError(f"Unknown assignment type: {self.assigment_type}", self.line, self.col, self.program)
            left_evaluated = self.identifier.eval(env)
            right_evaluated = self.expr.eval(env)
            self.check_compatibility(left_evaluated.type, right_evaluated.type, self.assigment_type)
            result_value, result_type = ASSIGNMENT_OPERATIONS[self.assigment_type](
                    left_evaluated, right_evaluated
                )
            env[self.identifier.identifier] = EvaluatedExpression(
                result_value,
                result_type,
                self.line,
                self.col,
                self.program,
            )
        else:
            env[self.identifier.identifier] = self.expr.eval(env)

    def __str__(self):
        return f"ASSIGN {self.identifier.identifier}: {{{self.expr}}}"

    def __repr__(self):
        return self.__str__()
    
    def pretty_string(self):
        return indent("{\nASSIGN\n" + self.identifier.identifier + "=\n" + self.expr.pretty_string() + "\n}")

    def check_compatibility(self, left_type: str, right_type: str, assignment_type: str) -> None:
        if left_type not in ASSIGNMENT_OPERATOR_TYPE_SUPPORT[assignment_type] or right_type not in ASSIGNMENT_OPERATOR_TYPE_SUPPORT[assignment_type]:
            raise LanguageError(f"Assignment type {assignment_type} is not compatible with types {left_type} and {right_type}", self.line, self.col, self.program)
        if left_type != right_type:
            raise LanguageError(f"Cannot perform mixed-type operation on {left_type} and {right_type}. Cast one of them to the other.", self.line, self.col, self.program)


class ElseStatement(Statement):
    # Else statements can have a condition (elif) and a follow-up else, if so
    def __init__(
        self,
        block: BlockStatement,
        condition: Expression | None = None,
        follow_up_else: Statement | None = None,
        line: int = 0,
        col: int = 0,
        program: str = "",
    ) -> None:
        super().__init__("else", line, col, program)
        self.block = block
        self.condition = condition
        self.follow_up_else = follow_up_else

    def eval(self, env: dict):
        if self.condition:
            evaluated_condition = self.condition.eval(env)
            if evaluated_condition.type != "boolean":
                raise LanguageError(f"Condition must be a boolean, got {evaluated_condition.type}. Cast it to a boolean with 'as boolean'", self.condition.line, self.condition.col, self.program)
            if evaluated_condition.value:
                self.block.eval(env)
            else:
                if self.follow_up_else:
                    self.follow_up_else.eval(env)
        else:
            self.block.eval(env)

    def __str__(self):
        return (
            f"ELIF ({self.condition}) THEN {self.block}"
            if self.condition
            else f"ELSE {self.block}"
        )

    def __repr__(self):
        return self.__str__()
    
    def pretty_string(self):
        return indent("{\n" + ("ELIF:\n" + self.condition.pretty_string() if self.condition else "ELSE:\n") + "THEN:\n" + self.block.pretty_string() + "}")

class IfStatement(Statement):
    def __init__(
        self,
        condition: Expression,
        block: BlockStatement,
        else_statement: ElseStatement | None = None,
        line: int = 0,
        col: int = 0,
        program: str = "",
    ) -> None:
        super().__init__("if", line, col, program)
        self.condition = condition
        self.block = block
        self.else_statement = else_statement

    def eval(self, env: dict):
        evaluated_condition = self.condition.eval(env)
        if evaluated_condition.type != "boolean":
            raise LanguageError(f"Condition must be a boolean, got {evaluated_condition.type}. Cast it to a boolean with 'as boolean'", self.condition.line, self.condition.col, self.program)
        if evaluated_condition.value:
            self.block.eval(env)
        elif self.else_statement:
            self.else_statement.eval(env)

    def __str__(self):
        return f"IF ({self.condition}) THEN {self.block} {self.else_statement if self.else_statement else ''}"

    def __repr__(self):
        return self.__str__()

    def pretty_string(self):
        return indent("{\nIF\n" + self.condition.pretty_string() + "THEN\n" + self.block.pretty_string() + (self.else_statement.pretty_string() if self.else_statement else "") + "\n}")