# ALL MY TODOS IN ONE PLACE:


# TODO: add returns
# TODO: Add in-language types to expressions (static typing)
# TODO: add objects or structs or something

# TODO: add some more non-standard literals for fun
# TODO: make a core lib in the language
# TODO: Rearrange parser's methods to make a followable flow
# TODO: add in-language none type

from shared import *
from lexer import Lexer
from nodes import *


# The way functions are scoped, no variable you change inside the function affects the outside


class Parser:
    def __init__(self, tokens: list, program: str):
        self.tokens = tokens
        self.program = program
        self.pos = 0
        self.AST = []

    def get_error(self, message: str):
        return ParserError(
            message, self.getNextToken().line, self.getNextToken().col, self.program
        )

    def getNextToken(self) -> Token:
        try:
            return self.tokens[self.pos]
        except IndexError:
            lines = self.program.split("\n")
            raise ImplementationError(
                "Unexpected end of file",
                len(lines) - 1,
                len(lines[len(lines) - 1]) - 1,
                self.program,
            )

    def checkNextToken(
        self, allowed_type: str | None = None, value: str | None = None
    ) -> bool:
        if (self.getNextToken().type == allowed_type or not allowed_type) and (
            self.getNextToken().value == value or not value
        ):
            return True
        else:
            return False

    def eat(self, allowed_type: str | None = None, value: str | None = None) -> Token:
        if self.checkNextToken(allowed_type, value):
            tok = self.getNextToken()
            self.pos += 1
            return tok
        else:
            raise self.get_error(
                "Expected "
                + (str(allowed_type) if allowed_type else "")
                + (" " + str(value) if value else "")
                + ", got "
                + self.getNextToken().type
                + " "
                + self.getNextToken().value
            )

    def literal_string(self) -> LiteralExpression:
        token = self.eat("literal_string")
        return LiteralExpression(
            token.value, "string", token.line, token.col, self.program
        )

    def literal_number(self) -> LiteralExpression:
        token = self.eat("literal_number")
        return LiteralExpression(
            token.value, "number", token.line, token.col, self.program
        )

    def literal_boolean(self) -> LiteralExpression:
        token = self.eat("literal_boolean")
        return LiteralExpression(
            token.value, "boolean", token.line, token.col, self.program
        )

    def literal(self) -> LiteralExpression:
        if self.checkNextToken("literal_string"):
            return self.literal_string()
        elif self.checkNextToken("literal_number"):
            return self.literal_number()
        elif self.checkNextToken("literal_boolean"):
            return self.literal_boolean()
        else:
            raise self.get_error(f"Unknown literal type: {self.getNextToken().type}")

    def identifier_name_list(self) -> list[str]:
        if not self.checkNextToken("identifier"):
            return []
        identifiers = [self.eat("identifier").value]
        while self.checkNextToken("misc_symbol", "comma"):
            self.eat()
            identifiers.append(self.eat("identifier").value)
        return identifiers

    def expression_list(self) -> list[Expression]:
        bracket_kind = self.eat("open_bracket").value
        if self.checkNextToken("close_bracket", bracket_kind):
            self.eat()
            return []
        expressions = [self.expression()]
        while self.checkNextToken("misc_symbol", "comma"):
            self.eat()
            expressions.append(self.expression())
        try:
            self.eat("close_bracket", bracket_kind)
        except:
            raise self.get_error(
                f"Expected a right bracket at the end of expression list, got {self.getNextToken().type}"
            )
        return expressions

    def function(self) -> Function:
        fn_token = self.eat("keyword_function")
        args = self.identifier_name_list()
        body = self.block()
        return Function(args, body, fn_token.line, fn_token.col, self.program)

    def identifier_reference(self) -> IdentifierRefrence:
        identifier = self.eat("identifier")
        return IdentifierRefrence(
            identifier.value, identifier.line, identifier.col, self.program
        )

    def primitive(
        self,
    ) -> IdentifierRefrence | LiteralExpression | FunctionCall | Function:
        if self.getNextToken().type == "identifier":
            if self.tokens[self.pos + 1].type == "open_bracket":
                return self.function_call(self.identifier_reference(), self.getNextToken().value)
            return self.identifier_reference()
        elif self.checkNextToken("keyword_function"):
            return self.anonymous_function_or_call()
        else:
            return self.literal()

    def expression_term(self) -> Expression:
        # A term is either a primitive or a parenthetical expression with any number of unary operators
        while (
            self.getNextToken().type == "unary_operator"
            or self.getNextToken().type == "ambiguous_operator"
        ):
            op = self.eat()
            return UnaryExpression(
                op.value, self.expression_term(), op.line, op.col, self.program
            )
        if self.checkNextToken("open_bracket"):
            bracket_kind = self.getNextToken().value
            self.eat()
            term = self.expression()
            if self.checkNextToken("close_bracket", bracket_kind):
                self.eat()
                return term
            else:
                raise self.get_error(f"Unclosed parentetical expression")
        else:
            term = self.primitive()
            return term

    def careful_get_precedence(self, token: Token) -> int | None:
        if self.is_binary_operator(token):
            return BINARY_OPERATOR_PRECEDENCE[token.value]
        else:
            return None
        
    def is_binary_operator(self, token: Token) -> bool:
        return token.type in ("binary_operator", "ambiguous_operator")
    
    def get_alike_expressions(self) -> tuple[list[Expression], list[str]]:
        # Returns a list of same-precedence expressions and the operators that separate them
        # Get lists of top-level terms and operators
        terms, operators = self.get_terms_and_operators()
        print(terms, operators, "HIIIIIIII")
        if len(operators) == 0:
            return terms, operators
        alike_expressions = []
        transition_operators = []
        precedence_level = BINARY_OPERATOR_PRECEDENCE[operators[0]]
        current_expr = terms.pop(0)
        while len(operators) > 0:
            # If the next term is followed by a higher-precedence operator (or if the current operator is lower precedence), put this expression on the list and start a new one
            # Putting the current operator on the list since it's transitoinal (the lower precedence of the two at the transition)
            if BINARY_OPERATOR_PRECEDENCE[operators[0]] < precedence_level:
                transition_operators.append(operators.pop(0))
                alike_expressions.append(current_expr)
                current_expr = terms.pop(0)
                precedence_level = BINARY_OPERATOR_PRECEDENCE[transition_operators[-1]]
            elif len(operators) > 1 and BINARY_OPERATOR_PRECEDENCE[operators[1]] > precedence_level:
                transition_operators.append(operators.pop(0))
                alike_expressions.append(current_expr)
                current_expr = terms.pop(0)
                precedence_level = BINARY_OPERATOR_PRECEDENCE[operators[0]]
            else:
                current_expr = BinaryExpression(current_expr, operators.pop(0), terms.pop(0), current_expr.line, current_expr.col, self.program)
        alike_expressions.append(current_expr)
        print(alike_expressions, transition_operators)
        return alike_expressions, transition_operators
    
    def get_terms_and_operators(self) -> tuple[list[Expression], list[str]]:
        # Returns a list all terms and the operators that separate them in the current expression
        operators = []
        terms = [self.expression_term()]
        while self.is_binary_operator(self.getNextToken()):
            operators.append(self.eat().value)
            terms.append(self.expression_term())
        return terms, operators
    
    def get_highest_precedence_index(self, operators: list[str]) -> int:
        highest_precedence = 0
        idx = 0
        for i, op in enumerate(operators):
            if BINARY_OPERATOR_PRECEDENCE[op] > highest_precedence:
                highest_precedence = BINARY_OPERATOR_PRECEDENCE[op]
                idx = i
        return idx

    def merge_expression_transitions(self, expressions: list[Expression], operators: list[str]) -> list[Expression]:
        # Because higner-precedence operators are nested deeper, we need to put the deepest ones in first'
        print(expressions, operators, "LOLOLOLOLOLOLOLO")
        while len(operators) > 0:
            idx = self.get_highest_precedence_index(operators)
            op = operators[idx]
            del operators[idx]
            # Merge the terms on either side of the operator
            expressions[idx] = BinaryExpression(expressions[idx], op, expressions.pop(idx + 1), expressions[idx].line, expressions[idx].col, self.program)
        print(expressions, "TESTTTTTTTTT")
        return expressions[0]


    def expression(self) -> Expression:
        """
        This is a pretty involved process
        There's probably a better way to parse expressions with precedence, but I like this because I figured it out on my own.
        - Basically, we first take every series of operations with the same precedence and merge them into a single expression left to right
        - Now we have a list of expressions separated by operators with precedence-jumps
        - Now we take the expression after the highest-precedence operator and merge it with the expression before it
        - We iterate this until we have a single expression left
        I think you can see why this respects precedence, since we merge (effectively "applying") higher-precedence operators first
        """
        expressions, operators = self.get_alike_expressions()
        expr = self.merge_expression_transitions(expressions, operators)
        return expr

    def block(self) -> BlockStatement:
        if not self.checkNextToken("misc_symbol", "open_brace"):
            raise self.get_error("Expected a block but got something else")
        open_brace = self.eat()
        blk = []
        while not self.checkNextToken("misc_symbol", "close_brace"):
            if self.getNextToken().type == "EOS":
                raise self.get_error("Unclosed block")
            blk.append(self.statement())
            # Every statemend in a block must end with a dot
            self.eat("misc_symbol", "dot")
        self.eat("misc_symbol", "close_brace")
        return BlockStatement(blk, open_brace.line, open_brace.col, self.program)

    def print_statement(self) -> PrintStatement:
        print_token = self.eat("keyword_print")
        expr = self.expression()
        return PrintStatement(expr, print_token.line, print_token.col, self.program)

    def else_statement(self) -> ElseStatement:
        else_token = self.eat("keyword_else")
        return ElseStatement(
            self.block(), None, else_token.line, else_token.col, self.program
        )

    def elif_statement(self) -> ElseStatement:
        elif_token = self.eat("keyword_elif")
        condition = self.expression()
        if not self.checkNextToken("misc_symbol", "comma"):
            raise self.get_error(
                "Expected a comma after the condition but got something else"
            )
        self.eat()
        block = self.block()
        followup = None
        if self.checkNextToken("keyword_elif"):
            followup = self.elif_statement()
        elif self.checkNextToken("keyword_else"):
            followup = self.else_statement()
        return ElseStatement(
            block, condition, followup, elif_token.line, elif_token.col, self.program
        )

    def if_statement(self) -> IfStatement:
        if_token = self.eat("keyword_if")
        condition = self.expression()
        if not self.checkNextToken("misc_symbol", "comma"):
            raise self.get_error(
                "Expected a comma after the condition but got something else"
            )
        self.eat()
        block = self.block()
        followup = None
        if self.getNextToken().type == "keyword_elif":
            followup = self.elif_statement()
        elif self.getNextToken().type == "keyword_else":
            followup = self.else_statement()
        return IfStatement(
            condition, block, followup, if_token.line, if_token.col, self.program
        )

    def while_statement(self) -> WhileStatement:
        while_token = self.eat("keyword_while")
        condition = self.expression()
        if not self.checkNextToken("misc_symbol", "comma"):
            raise self.get_error(
                "Expected a comma after the condition but got something else"
            )
        self.eat()
        block = self.block()
        return WhileStatement(
            condition, block, while_token.line, while_token.col, self.program
        )

    def for_statement(self) -> ForStatement:
        for_token = self.eat("keyword_for")
        condition = self.expression()
        if not self.checkNextToken("misc_symbol", "comma"):
            raise self.get_error(
                "Expected a comma after the condition but got something else"
            )
        self.eat()
        block = self.block()
        if not self.checkNextToken("misc_symbol", "comma"):
            raise self.get_error(
                "Expected a comma before the modifier but got something else"
            )
        self.eat()
        modifier_statement = self.statement()
        return ForStatement(
            condition,
            block,
            modifier_statement,
            for_token.line,
            for_token.col,
            self.program,
        )

    def error_statement(self) -> ErrorStatement:
        err = self.eat("keyword_error")
        message = self.expression()
        return ErrorStatement(message, err.line, err.col, self.program)

    def function_call(self, function: Function, identifier: str | None = None) -> FunctionCall:
        start_tok = self.getNextToken()
        args = self.expression_list()
        return FunctionCall(function, args, identifier, start_tok.line, start_tok.col, self.program)

    # This is a primitive, even though functions are technically their own thing
    def anonymous_function_or_call(self) -> Function | FunctionCall:
        kwd = self.eat("keyword_function")
        args = self.identifier_name_list()
        body = self.block()
        if self.checkNextToken("open_bracket"):
            return self.function_call(Function(args, body, kwd.line, kwd.col, self.program))
        else:
            return Function(args, body, kwd.line, kwd.col, self.program)

    def assignment_statement(self) -> AssignmentStatement:
        identifier = self.eat("identifier").value
        assignment_operator = self.eat("assignment_operator")
        if self.getNextToken().type == "keyword_function":
            expr = self.function()
        else:
            expr = self.expression()
        return AssignmentStatement(
            identifier,
            expr,
            assignment_operator.value,
            assignment_operator.line,
            assignment_operator.col,
            self.program,
        )

    def statement(self) -> Statement:
        next_token_type = self.getNextToken().type
        match next_token_type:
            case "keyword_print":
                statement = self.print_statement()
            case "keyword_if":
                statement = self.if_statement()
            case "keyword_while":
                statement = self.while_statement()
            case "keyword_for":
                statement = self.for_statement()
            case "keyword_error":
                statement = self.error_statement()
            case "keyword_function":
                # Anonymous function or call
                statement = self.anonymous_function_or_call()
            case "identifier":
                try:
                    is_function_call = self.tokens[self.pos + 1].type == "open_bracket"
                except IndexError:
                    raise self.get_error(
                        "Expected a statement but got just an identifier"
                    )
                if is_function_call:
                    identifier = self.getNextToken().value
                    function_ref = self.identifier_reference()
                    # TODO once we have types
                    # if function_ref.type != "function_call":
                    #     raise self.get_error(f"Type {function_ref.type} is not callable")
                    statement = self.function_call(function_ref, identifier)
                else:
                    statement = self.assignment_statement()
            case _:
                raise self.get_error("Unknown statement type: " + next_token_type)
        return statement

    def parse(self) -> list[Statement]:
        while self.getNextToken().type != "EOS":
            self.AST.append(self.statement())
        self.eat("EOS")
        return self.AST


class Evaluator:
    def __init__(self, AST: list[Statement], program: str, env: dict[str, Expression]):
        self.program = program
        self.AST = AST
        self.env = env

    def eval(self) -> dict[str, Expression]:
        self.print_ast()
        for statement in self.AST:
            statement.eval(self.env)
        return self.env

    def print_ast(self):
        for statement in self.AST:
            print(statement.pretty_string())
            # print(statement)


def interpret(program, env: dict[str, Expression] = {}):
    parser = Parser(Lexer(program).tokenize(), program)
    parser.parse()
    evaluator = Evaluator(parser.AST, program, env)
    evaluator.eval()


if __name__ == "__main__":
    with open("test.dlg", "r") as f:
        program = f.read()
        interpret(program, {})
    with open("tests.dlg", "r") as f:
        program = f.read()
        interpret(program, {})
    env = {}
    while True:
        program = input(">")
        interpret(program, env)
