from shared import *
# TODO: Add in-language types to expressions
# TODO: add order of operations
# TODO: add more error types for operators (currently always just "invalid operation", sometimes masks things like overflow)
# TODO: fix "-" unary operator
# TODO: make a core lib in the language
# TODO: add static typing
# TODO: add objects or structs or something

class Statement:
    def __init__(self, type):
        self.type = type

    def __str__(self):
        return f"[{self.type} statement]"

    def __repr__(self):
        return self.__str__()
    
class PrintStatement(Statement):
    def __init__(self, expr):
        super().__init__("print")
        self.expr = expr
    
    def eval(self, env):
        evaluated_expr = self.expr.eval(env)
        print(evaluated_expr)

    def __str__(self):
        return f"PRINT {self.expr}"


class WhileStatement(Statement):
    def __init__(self, condition, block):
        super().__init__("while")
        self.condition = condition
        self.block = block
    
    def eval(self, env):
        while self.condition.eval(env):
            self.block.eval(env)
    
    def __str__(self):
        return f"WHILE {self.condition} DO {self.block}"

class ForStatement(Statement):
    def __init__(self, condition, block, modifier_statement):
        super().__init__("for")
        self.condition = condition
        self.modifier_statement = modifier_statement
        self.block = block

    def eval(self, env):
        while self.condition.eval(env):
            self.block.eval(env)
            self.modifier_statement.eval(env)
        
    def __str__(self):
        return f"FOR {self.condition} DO {self.block} MODIFIER: {self.modifier_statement}"

class ErrorStatement(Statement):
    def __init__(self, message, line, col, program):
        super().__init__("error")
        self.message = message
        self.line = line
        self.col = col
        self.program = program
    
    def eval(self, env):
        raise LanguageError(self.message.eval(env), self.line, self.col, self.program)
    
    def __str__(self):
        return f"ERROR {self.message}"

class FunctionCallStatement(Statement):
    # TODO
    def __init__(self, identifier, args):
        super().__init__("function_call")
        self.identifier = identifier
        self.args = args
    
    def eval(self, env):
        raise NotImplementedError("Function calls not implemented", 0, 0, "NOT IMPLEMENTED")

class AssignmentStatement(Statement):
    def __init__(self, identifier, expr):
        super().__init__("assignment")
        self.identifier = identifier
        self.expr = expr
    
    def eval(self, env):
        env[self.identifier] = self.expr.eval(env)
    
    def __str__(self):
        return f"ASSIGN {self.identifier}: {self.expr}"

class BlockStatement(Statement):
    def __init__(self, statements):
        super().__init__("block")
        self.statements = statements
    
    def eval(self, env):
        for statement in self.statements:
            statement.eval(env)
    
    def __str__(self):
        return f"BLOCK: {self.statements}"
    
class IfStatement(Statement):
    def __init__(self, condition, block, else_statement=None):
        super().__init__("if") 
        self.condition = condition
        self.block = block
        self.else_statement = else_statement
    
    def eval(self, env):
        if self.condition.eval(env):
            self.block.eval(env)
        elif self.else_statement:
            self.else_statement.eval(env)

    def __str__(self):
        return f"IF {self.condition} THEN {self.block} ELSE {self.else_statement}"


class ElseStatement(Statement):
    # Else statements can have a condition (elif) and a follow-up else, if so
    def __init__(self, block, condition=None, follow_up_else=None):
        super().__init__("else")
        self.block = block
        self.condition = condition
        self.follow_up_else = follow_up_else
    
    def eval(self, env):
        if self.condition:
            if self.condition.eval(env):
                self.block.eval(env)
            else:
                if self.follow_up_else:
                    self.follow_up_else.eval(env)
        else:
            self.block.eval(env)

    def __str__(self):
        return f"ELIF {self.condition} THEN {self.block}" if self.condition else f"ELSE {self.block}"

class Expression:
    def __init__(self, type, line, col, program):
        self.type = type
        self.line = line
        self.col = col
        self.program = program
    
    def __str__(self):
        return f"({self.type})"

    def __repr__(self):
        return self.__str__()

class BinaryExpression(Expression):
    def __init__(self, left, operator, right, line, col, program):
        super().__init__("binary_expression", line, col, program)
        self.left = left
        self.operator = operator
        self.right = right
    
    def eval(self, env):
        # becuase the right expression is always more buried than the left currently, there's right-precedence
        # (Because the rightmost expression has to be already evaluated to be able to evaluate the leftmost)
        try:
            return OPERATIONS[self.operator](self.left.eval(env), self.right.eval(env))
        except LanguageError as e:
            # Prevents errors from bubbling up and giving very uninformative messages
            raise e
        except:
            raise LanguageError(f"Invalid operation: {self.left} {self.operator} {self.right}", self.line, self.col, self.program)
    
    def __str__(self):
        return f"[{super().__str__()} {self.left} {self.operator} {self.right}]"
    
class UnaryExpression(Expression):
    def __init__(self, operator, expr, line, col, program):
        super().__init__("unary_expression", line, col, program)
        self.operator = operator
        self.expr = expr
    
    def eval(self, env):
        try:
            return OPERATIONS[self.operator](self.expr.eval(env))
        except LanguageError as e:
            # Prevents errors from bubbling up and giving very uninformative messages
            raise e
        except:
            raise LanguageError(f"Invalid operation: {self.operator} {self.expr}", self.line, self.col, self.program)
    
    def __str__(self):
        return f"[{super().__str__()} {self.operator} {self.expr}]"
    
class LiteralExpression(Expression):
    # TODO: type
    def __init__(self, literal, type, line, col, program):
        super().__init__(f"literal {type}", line, col, program)
        self.literal = literal
    
    def eval(self, env):
        return self.literal
    
    def __str__(self):
        return f"[{super().__str__()} {self.literal}]"
    
class IdentifierRefrence(Expression):
    def __init__(self, identifier, line, col, program):
        super().__init__("identifier", line, col, program)
        self.identifier = identifier
    
    def eval(self, env):
        try:
            return env[self.identifier]
        except KeyError:
            raise LanguageError(f"Identifier not found: {self.identifier}", self.line, self.col, self.program)
    
    def __str__(self):
        return f"[{super().__str__()} {self.identifier}]"
    

class Function():
    def __init__(self, args, body):
        # args is a list of identifier strings
        self.args = args
        self.body = body
    
    def __str__(self):
        return f"[FUNCTION {self.args} => {self.body}]"
    
    def __repr__(self):
        return self.__str__()
    
    def eval(self, env):
        return self
    
    def call(self, env, args):
        if len(self.args) != len(args):
            raise LanguageError(f"Wrong number of arguments for function {self.args} (expected {len(self.args)}, got {len(args)})", self.line, self.col, self.program)
        scope = env.copy()
        for arg, value in zip(self.args, args):
            scope[arg] = value.eval(scope)
        return self.body.eval(scope)
    

class FunctionCall(Expression):
    def __init__(self, identifier, args, line, col, program):
        # TODO: add anonymous function calls
        super().__init__("function_call", line, col, program)
        self.identifier = identifier
        self.args = args
    
    def eval(self, env):
        try:
            return env[self.identifier].call(env, self.args)
        except KeyError:
            raise LanguageError(f"Function not found: {self.identifier}", self.line, self.col, self.program)
        except LanguageError as e:
            # Prevents errors from bubbling up and giving very uninformative messages
            raise e
        
    def __str__(self):
        return f"[FUNCTION_CALL {self.identifier} {self.args}]"

