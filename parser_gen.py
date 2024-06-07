# ALL MY TODOS IN ONE PLACE:


# TODO: refactor evaluator
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
        return ParserError(message, self.getNextToken().line, self.getNextToken().col, self.program)

    def getNextToken(self) -> Token:
        try:
            return self.tokens[self.pos]
        except IndexError:
            lines = self.program.split("\n")
            raise ImplementationError("Unexpected end of file", len(lines) - 1, len(lines[len(lines) - 1]) - 1, self.program)
    
    def checkNextToken(self, allowed_type=None, value=None) -> bool:
        if (self.getNextToken().type == allowed_type or not allowed_type) and (self.getNextToken().value == value or not value):
            return True
        else:
            return False

    def eat(self, allowed_type=None, value=None) -> Token:
        if self.checkNextToken(allowed_type, value):
            tok = self.getNextToken()
            self.pos += 1
            return tok
        else:
            raise self.get_error("Expected " + (str(allowed_type) if allowed_type else "") + (" " + str(value) if value else "") + ", got " + self.getNextToken().type + " " + self.getNextToken().value)
        
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
            raise self.get_error(f"Expected a right bracket at the end of expression list, got {self.getNextToken().type}")
        return expressions

    def function(self) -> Function:
        self.eat("keyword_function")
        args = self.identifier_name_list()
        body = self.block()
        return Function(args, body)

    def identifier_reference(self) -> IdentifierRefrence:
        identifier = self.eat("identifier")
        return IdentifierRefrence(
            identifier.value, identifier.line, identifier.col, self.program
        )

    def primitive(self):
        if self.getNextToken().type == "identifier":
            if self.tokens[self.pos + 1].type == "open_bracket":
                return self.function_call(self.identifier_reference())
            return self.identifier_reference()
        elif self.checkNextToken("keyword_function"):
            return self.anonymous_function_or_call()
        else:
            return self.literal()
        

    def expression_term(self):
        # A term is either a primitive or a parenthetical expression with any number of unary operators
        while self.getNextToken().type == "unary_operator" or self.getNextToken().type == "ambiguous_operator":
            op = self.eat()
            return UnaryExpression(op.value, self.expression_term(), op.line, op.col, self.program)
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
        
    def careful_get_precedence(self, token):
        if token.type in ("binary_operator", "ambiguous_operator"):
            return BINARY_OPERATOR_PRECEDENCE[token.value]
        else:
            return None

    def alike_expression(self, op=None):
        # A chain of operations where all operators are of the same precedence
        expr = self.expression_term()
        if op:
            # If this isn't the first alike_expression, its precedence is the precedence of the operator that stopped the last one
            precedence = BINARY_OPERATOR_PRECEDENCE[op.value]
        else:
            # If it's the first alike_expression, its precedence is the precedence of the first operator
            precedence = self.careful_get_precedence(self.getNextToken())
        # While there's a next operator and its precedence is the same
        while self.careful_get_precedence(self.getNextToken()) != None and self.careful_get_precedence(self.getNextToken()) == precedence:
            op = self.eat()
            term = self.expression_term()
            expr = BinaryExpression(expr, op.value, term, op.line, op.col, self.program)
        return expr

    def expression(self):
        expr = self.alike_expression()
        # While there's a next operator
        while self.careful_get_precedence(self.getNextToken()) != None:
            op = self.eat()
            expr2 = self.alike_expression(op)
            # We messed up - the last term of expr should have belonged to the expr2 because it had higher precedence
            if BINARY_OPERATOR_PRECEDENCE[op.value] > BINARY_OPERATOR_PRECEDENCE[expr.operator]:
                # Pop off that term and give it to expr2
                expr2 = BinaryExpression(expr.right, op.value, expr2, expr2.line, expr2.col, self.program)
                op = Token("binary_operator", expr.operator, expr.line, expr.col)
                expr = expr.left
            expr = BinaryExpression(expr, op.value, expr2, op.line, op.col, self.program)
        return expr

    def block(self):
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

    def print_statement(self):
        print_token = self.eat("keyword_print")
        expr = self.expression()
        return PrintStatement(expr, print_token.line, print_token.col, self.program)

    def else_statement(self):
        else_token = self.eat("keyword_else")
        return ElseStatement(self.block(), None, else_token.line, else_token.col, self.program)
    
    def elif_statement(self):
        elif_token = self.eat("keyword_elif")
        condition = self.expression()
        if not self.checkNextToken("misc_symbol", "comma"):
            raise self.get_error("Expected a comma after the condition but got something else")
        self.eat()
        block = self.block()
        followup = None
        if self.checkNextToken("keyword_elif"):
            followup = self.elif_statement()
        elif self.checkNextToken("keyword_else"):
            followup = self.else_statement()
        return ElseStatement(block, condition, followup, elif_token.line, elif_token.col, self.program)


    def if_statement(self):
        if_token = self.eat("keyword_if")
        condition = self.expression()
        if not self.checkNextToken("misc_symbol", "comma"):
            raise self.get_error("Expected a comma after the condition but got something else")
        self.eat()
        block = self.block()
        followup = None
        if self.getNextToken().type == "keyword_elif":
            followup = self.elif_statement()
        elif self.getNextToken().type == "keyword_else":
            followup = self.else_statement()
        return IfStatement(condition, block, followup, if_token.line, if_token.col, self.program)
    
    def while_statement(self):
        while_token = self.eat("keyword_while")
        condition = self.expression()
        if not self.checkNextToken("misc_symbol", "comma"):
            raise self.get_error("Expected a comma after the condition but got something else")
        self.eat()
        block = self.block()
        return WhileStatement(condition, block, while_token.line, while_token.col, self.program)

    def for_statement(self):
        for_token = self.eat("keyword_for")
        condition = self.expression()
        if not self.checkNextToken("misc_symbol", "comma"):
            raise self.get_error("Expected a comma after the condition but got something else")
        self.eat()
        block = self.block()
        if not self.checkNextToken("misc_symbol", "comma"):
            raise self.get_error("Expected a comma before the modifier but got something else")
        self.eat()
        modifier_statement = self.statement()
        return ForStatement(condition, block, modifier_statement, for_token.line, for_token.col, self.program)

    def error_statement(self):
        err =self.eat("keyword_error")
        message = self.expression()
        return ErrorStatement(message, err.line, err.col, self.program)
    
    def function_call(self, function):
        start_tok = self.getNextToken()
        args = self.expression_list()
        return FunctionCall(function, args, start_tok.line, start_tok.col, self.program)
    
    def anonymous_function_or_call(self):
        kwd = self.eat("keyword_function")
        args = self.identifier_name_list()
        body = self.block()
        if self.checkNextToken("open_bracket"):
            return self.function_call(Function(args, body))
        else:
            return Function(args, body)


    def assignment_statement(self):
        identifier = self.eat("identifier").value
        assignment_operator = self.eat("assignment_operator")
        if self.getNextToken().type == "keyword_function":
            expr = self.function()
        else:
            expr = self.expression()
        return AssignmentStatement(identifier, expr, assignment_operator.value, assignment_operator.line, assignment_operator.col, self.program)

    def statement(self):
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
                    raise self.get_error("Expected a statement but got just an identifier")
                if is_function_call:
                    function_ref = self.identifier_reference()
                    # TODO once we have types
                    # if function_ref.type != "function_call":
                    #     raise self.get_error(f"Type {function_ref.type} is not callable")
                    statement = self.function_call(function_ref)
                else:
                    statement = self.assignment_statement()
            case _:
                raise self.get_error("Unknown statement type: " + next_token_type)
        return statement

    def parse(self):
        while self.getNextToken().type != "EOS":
            self.AST.append(self.statement())
        self.eat("EOS")
        return self.AST


class Evaluator:
    def __init__(self, AST, program, env):
        self.program = program
        self.AST = AST
        self.env = env

    def eval(self):
        self.print_ast()
        for statement in self.AST:
            statement.eval(self.env)
        return self.env
    
    def print_ast(self):
        for statement in self.AST:
            print(statement.pretty_string(0))


def interpret(program, env={}):
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