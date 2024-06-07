# TODO: add returns
# TODO: refactor EVERYTHING
# TODO: ESPECIALLY the way i implemented functions
# TODO: return statements and function calls as expressions
# TODO: make functions passable as naked expressions
# TODO: make functions work with no args
# TODO: scopes for functions if i didn't already add them

from shared import *
from lexer import Lexer
from nodes import *


def print_ast(AST):
    for statement in AST:
        print(statement)

class Evaluator:
    def __init__(self, AST, program, env):
        self.program = program
        self.AST = AST
        self.env = env

    def eval(self):
        print_ast(self.AST)
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
    def __init__(self, tokens, program):
        self.tokens = tokens
        self.program = program
        self.pos = 0
        self.AST = []

    def getNextToken(self):
        return self.tokens[self.pos]
    
    def lookAhead(self):
        return self.tokens[self.pos + 1]

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
        
    def eat_value(self, value):
        token = self.getNextToken()
        if token.value == value:
            self.pos += 1
            return token
        else:
            raise ParserError(
                f"Expected {value}, got {token.value}",
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

    def identifier_name_list(self):
        if self.getNextToken().type != "identifier":
            return []
        identifiers = [self.eat("identifier").value]
        while self.getNextToken().value == "comma":
            self.eat_value("comma")
            identifiers.append(self.eat("identifier").value)
        return identifiers
    
    def expression_list(self):
        terms = [self.expression()]
        while self.getNextToken().value == "comma":
            self.eat_value("comma")
            terms.append(self.expression())
        return terms

    def function(self):
        self.eat("keyword_function")
        args = self.identifier_name_list()
        body = self.block()
        return Function(args, body)

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
        if self.getNextToken().type == "bracket":
            bracket_kind = self.getNextToken().value
            self.eat_value(bracket_kind)
            term = self.expression()
            self.eat_value(bracket_kind)
        else:
            term = self.primitive()
        return term

    def alike_expression(self):
        # A chain of operations where all operators are of the same precedence
        # Include the last item only if the following chain has a lower precedence
        expr = self.expression_term()
        precedence = BINARY_OPERATOR_PRECEDENCE[self.getNextToken().value] if self.getNextToken().type == "binary_operator" else None
        while self.getNextToken().type == "binary_operator" and BINARY_OPERATOR_PRECEDENCE[self.getNextToken().value] == precedence:
            op = self.eat("binary_operator")
            term = self.expression_term()
            expr = BinaryExpression(expr, op.value, term, op.line, op.col, self.program)
        return expr

    def expression(self):
        # Expression_term automatically handles parentheses grouping so all this has to do is apply order of operators to black-box terms
        # As if they were primitives
        # To do this, we can treat each grouping of consecutive same operations as its own expression, merging them into one node
        # And then apply the transitional operations to those merged nodes
        expr = self.alike_expression()
        while self.getNextToken().type == "binary_operator":
            op = self.eat("binary_operator")
            expr2 = self.alike_expression()
            # We messed up - the last term of expr should have belonged to the expr2 because it had higher precedence
            if BINARY_OPERATOR_PRECEDENCE[op.value] > BINARY_OPERATOR_PRECEDENCE[expr.operator]:
                # Pop off that term and give it to expr2
                expr2 = BinaryExpression(expr.right, op.value, expr2, expr2.line, expr2.col, self.program)
                op = Token("binary_operator", expr.operator, expr.line, expr.col)
                expr = expr.left
            expr = BinaryExpression(expr2, op.value, expr, op.line, op.col, self.program)
        return expr



    def block(self):
        open_brace = self.eat_value("open_brace")
        blk = []
        while self.getNextToken().value != "close_brace":
            blk.append(self.statement())
            if self.getNextToken().value != "close_brace":
                self.eat("keyword_then")
        self.eat_value("close_brace")
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
        self.eat_value("comma")
        block = self.block()
        if self.getNextToken().type == "keyword_elif":
            return ElseStatement(block, condition, self.elif_statement(), elif_token.line, elif_token.col, self.program)
        elif self.getNextToken().type == "keyword_else":
            return ElseStatement(block, condition, self.else_statement(), elif_token.line, elif_token.col, self.program)
        return ElseStatement(block, condition, None, elif_token.line, elif_token.col, self.program)


    def if_statement(self):
        if_token = self.eat("keyword_if")
        condition = self.expression()
        self.eat_value("comma")
        block = self.block()
        if self.getNextToken().type == "keyword_elif":
            return IfStatement(condition, block, self.elif_statement(), if_token.line, if_token.col, self.program)
        elif self.getNextToken().type == "keyword_else":
            return IfStatement(condition, block, self.else_statement(), if_token.line, if_token.col, self.program)
        return IfStatement(condition, block, None, if_token.line, if_token.col, self.program)
    
    def while_statement(self):
        while_token = self.eat("keyword_while")
        condition = self.expression()
        self.eat_value("comma")
        block = self.block()
        return WhileStatement(condition, block, while_token.line, while_token.col, self.program)

    def for_statement(self):
        for_token = self.eat("keyword_for")
        condition = self.expression()
        self.eat_value("comma")
        block = self.block()
        self.eat_value("comma")
        modifier_statement = self.statement()
        return ForStatement(condition, block, modifier_statement, for_token.line, for_token.col, self.program)

    def error_statement(self):
        line = self.getNextToken().line
        col = self.getNextToken().col
        self.eat("keyword_error")
        message = self.expression()
        return ErrorStatement(message, line, col, self.program)
    
    def function_call_statement(self):
        identifier = self.eat("identifier")
        self.eat_value("open_paren")
        args = self.expression_list()
        self.eat_value("close_paren")
        return FunctionCall(identifier.value, args, identifier.line, identifier.col, self.program)

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
            case "identifier":
                if self.lookAhead().value == "open_paren":
                    statement = self.function_call_statement()
                else:
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
    with open("test.dlg", "r") as f:
        program = f.read()
        interpret(program, {})
    env = {}
    while True:
        program = input(">")
        interpret(program, env)