# Implement multitype expressions later (eg, with coercion or something)
# TODO: pass program and line/col to nodes to allow evalErrors
# Maybe scope the language at some point (should be easy, just mess with the env in blocks)

# TODO: add parentheses to expressions
# TODO: add returns
# TODO: add error for unterminated block

from shared import *
from lexer import Lexer
from nodes import *


class Evaluator:
    def __init__(self, AST, program, env):
        self.program = program
        self.AST = AST
        self.env = env

    def eval(self):
        for statement in self.AST:
            statement.eval(self.env)
        return self.env


class Compiler:
    # TODO: add compile method to nodes that compiles to c
    # TODO: add dynamic typing and type-checking in the compiler
    def __init__(self, AST, program):
        self.program = program
        self.AST = AST

    def compile(self):
        for statement in self.AST:
            statement.compile(self.program)


class Parser:
    # TODO: merge elifs into else => if
    def __init__(self, tokens, program):
        self.tokens = tokens
        self.program = program
        self.pos = 0
        # The AST wil be a list of trees (each line is a tree)
        self.AST = []

    def getNextToken(self):
        return self.tokens[self.pos]

    def eat(self, allowed_types):
        token = self.getNextToken()
        if token.type in allowed_types:
            self.pos += 1
            return token
        else:
            raise ParserError(
                f"{'Expected one of: ' if type(allowed_types) == list else 'Expected '}{str(allowed_types)}, got "
                + self.getNextToken().type,
                self.getNextToken().line,
                self.getNextToken().col,
                self.program,
            )

    def literal_string(self):
        token = self.eat("literal_string")
        return LiteralExpression(
            token.value, "string", token.line, token.col, self.program
        )

    def literal_number(self):
        token = self.eat("literal_number")
        return LiteralExpression(
            token.value, "number", token.line, token.col, self.program
        )

    def literal_boolean(self):
        token = self.eat("literal_boolean")
        return LiteralExpression(
            token.value, "boolean", token.line, token.col, self.program
        )

    def literal(self):
        # Returns the next token as long as it's a literal
        if self.getNextToken().type == "literal_string":
            return self.literal_string()
        elif self.getNextToken().type == "literal_number":
            return self.literal_number()
        elif self.getNextToken().type == "literal_boolean":
            return self.literal_boolean()
        else:
            raise ParserError(
                f"Unknown literal type: {self.getNextToken().type}",
                self.getNextToken().line,
                self.getNextToken().col,
                self.program,
            )

    def identifier_reference(self):
        identifier = self.eat("identifier")
        return IdentifierRefrence(
            identifier.value, identifier.line, identifier.col, self.program
        )

    def primitive(self):
        # TODO: add function call as primitive type
        if self.getNextToken().type == "identifier":
            return self.identifier_reference()
        else:
            return self.literal()

    def expression_term(self):
        while self.getNextToken().type == "unary_operator":
            op = self.eat("unary_operator")
            term = UnaryExpression(op.value, self.expression_term(), op.line, op.col, self.program)
        if self.getNextToken().type == "open_paren":
            term = self.expression()
            self.eat("closing_paren")
        else:
            term = self.primitive()
        return term

    def expression(self):
        while self.getNextToken().type == "binary_operator":
            op = self.eat("binary_operator")

            


    def block(self):
        blk = []
        while self.getNextToken().type != "dot":
            blk.append(self.statement())
            if self.getNextToken().type != "dot":
                self.eat("keyword_then")
        self.eat("dot")
        return BlockStatement(blk)

    def print_statement(self):
        self.eat("keyword_print")
        expr = self.expression()
        return PrintStatement(expr)

    def if_statement(self):
        self.eat("keyword_if")
        condition = self.expression()
        self.eat("comma")
        block = self.block()
        if self.getNextToken().type == "keyword_else":
            self.eat("keyword_else")
            else_block = self.block()
        else:
            else_block = None
        return IfStatement(condition, block, else_block)

    def while_statement(self):
        self.eat("keyword_while")
        condition = self.expression()
        self.eat("comma")
        block = self.block()
        return WhileStatement(condition, block)

    def for_statement(self):
        self.eat("keyword_for")
        condition = self.expression()
        self.eat("comma")
        block = self.block()
        self.eat("comma")
        modifier_statement = self.statement()
        return ForStatement(condition, block, modifier_statement)

    def error_statement(self):
        line = self.getNextToken().line
        col = self.getNextToken().col
        self.eat("keyword_error")
        message = self.expression()
        return ErrorStatement(message, line, col, self.program)

    def assignment_statement(self):
        identifier = self.eat("identifier").value
        self.eat("assign")
        expr = self.expression()
        return AssignmentStatement(identifier, expr)

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
            case "identifier":
                # TODO: add function call and function definition
                # TYPES OF IDENTIFIER STATEMENT: ASSIGNMENT, FUNCTION CALL, FUNCTION DEFINITION
                statement = self.assignment_statement()
            case _:
                raise ParserError(
                    f"Unknown statement type: {next_token_type}",
                    self.getNextToken().line,
                    self.getNextToken().col,
                    self.program,
                )
        return statement

    def parse(self):
        while self.getNextToken().type != "EOS":
            self.AST.append(self.statement())
        self.eat("EOS")
        return self.AST




def interpret(program, env={}):
    parser = Parser(Lexer(program).tokenize(), program)
    parser.parse()
    evaluator = Evaluator(parser.AST, program, env)
    evaluator.eval()


if __name__ == "__main__":
    # with open("test.dlg", "r") as f:
    #     program = f.read()
    #     interpret(program, {})
    env = {}
    while True:
        program = input(">")
        interpret(program, env)