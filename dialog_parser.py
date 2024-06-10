# TODO: add more tests
# TODO: add some more non-standard literals for fun
# TODO: make a core lib in the language

from shared import *
from nodes import *

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
        # Get token at pos
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
        # Return true if the token has the right type and value
        if (self.getNextToken().type == allowed_type or not allowed_type) and (
            self.getNextToken().value == value or not value
        ):
            return True
        else:
            return False

    def eat(self, allowed_type: str | None = None, value: str | None = None) -> Token:
        # Take the next token and advance the pos if it matches the type and value, else throw an error
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
        
    def parse(self) -> list[Statement]:
        # Turn the tokenized program into a list of statements (AST)
        while self.getNextToken().type != "EOS":
            self.AST.append(self.statement())
        self.eat("EOS")
        return self.AST

    def statement(self, isBlock: bool = False) -> Statement:
        # Parse a single statement
        if self.checkNextToken("keyword", "print"):
            statement = self.print_statement()
        elif self.checkNextToken("keyword", "if"):
            statement = self.if_statement()
        elif self.checkNextToken("keyword", "while"):
            statement = self.while_statement()
        elif self.checkNextToken("keyword", "for"):
            statement = self.for_statement()
        elif self.checkNextToken("keyword", "error"):
            statement = self.error_statement()
        elif self.checkNextToken("keyword", "function_keyword"):
            statement = self.anonymous_function_or_call()
            if statement.type != "function_call":
                raise self.get_error("Uncalled anonymous function")
        elif self.checkNextToken("keyword", "import"):
            statement = self.import_statement()
        elif self.checkNextToken("keyword", "return"):
            if not isBlock:
                raise self.get_error("Return statement can only be used inside of a block")
            statement = self.return_statement()
        elif self.checkNextToken("identifier"):
            try:
                is_function_call = self.tokens[self.pos + 1].type == "open_bracket"
            except IndexError:
                raise self.get_error(
                    "Expected a statement but got just an identifier"
                )
            if is_function_call:
                identifier = self.getNextToken().value
                function_ref = self.identifier_reference()
                statement = self.function_call(function_ref, identifier)
            else:
                statement = self.assignment_statement()
        else:
            raise self.get_error("Unknown statement type: " + self.getNextToken().type)
        # Dots are optional. Useful if you want to cram multiple statements on one line
        if self.checkNextToken("misc_symbol", "dot"):
            self.eat()
        return statement
    
    def print_statement(self) -> PrintStatement:
        # Get a print statement node
        print_token = self.eat("keyword", "print")
        expr = self.expression()
        return PrintStatement(expr, print_token.line, print_token.col, self.program)

    def if_statement(self) -> IfStatement:
        # If statement
        if_token = self.eat("keyword", "if")
        condition = self.expression()
        if not self.checkNextToken("misc_symbol", "comma"):
            raise self.get_error(
                "Expected a comma after the condition but got something else"
            )
        self.eat()
        block = self.block()
        followup = None
        if self.checkNextToken("keyword", "elif"):
            followup = self.elif_statement()
        elif self.checkNextToken("keyword", "else"):
            followup = self.else_statement()
        return IfStatement(
            condition, block, followup, if_token.line, if_token.col, self.program
        )

    def else_statement(self) -> ElseStatement:
        # Else statement
        else_token = self.eat("keyword", "else")
        return ElseStatement(
            self.block(), None, else_token.line, else_token.col, self.program
        )

    def elif_statement(self) -> ElseStatement:
        # Else if statement
        elif_token = self.eat("keyword", "elif")
        condition = self.expression()
        if not self.checkNextToken("misc_symbol", "comma"):
            raise self.get_error(
                "Expected a comma after the condition but got something else"
            )
        self.eat()
        block = self.block()
        followup = None
        if self.checkNextToken("keyword", "elif"):
            followup = self.elif_statement()
        elif self.checkNextToken("keyword", "else"):
            followup = self.else_statement()
        return ElseStatement(
            block, condition, followup, elif_token.line, elif_token.col, self.program
        )

    def while_statement(self) -> WhileStatement:
        # While statement
        while_token = self.eat("keyword", "while")
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
        # For statement
        for_token = self.eat("keyword", "for")
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

    def block(self) -> BlockStatement:
        # Statement that contains a list of statements that evaluate when it's evaluated
        if not self.checkNextToken("misc_symbol", "block_start"):
            raise self.get_error("Expected a block but got something else")
        block_start = self.eat()
        blk = []
        while not self.checkNextToken("misc_symbol", "block_end"):
            if self.getNextToken().type == "EOS":
                raise self.get_error("Unclosed block")
            blk.append(self.statement(True))
        self.eat("misc_symbol", "block_end")
        return BlockStatement(blk, block_start.line, block_start.col, self.program)


    def error_statement(self) -> ErrorStatement:
        # Throw an error when evaluated
        err = self.eat("keyword", "error")
        message = self.expression()
        return ErrorStatement(message, err.line, err.col, self.program)

    def return_statement(self) -> ReturnStatement:
        # Return statement (only used in blocks, only does something in function calls)
        return_token = self.eat("keyword", "return")
        expr = self.expression()
        return ReturnStatement(expr, return_token.line, return_token.col, self.program)

    def import_statement(self) -> ImportStatement:
        # Inject the code at expr into the current scope and evaluate it
        import_token = self.eat("keyword", "import")
        expr = self.expression()
        return ImportStatement(expr, import_token.line, import_token.col, self.program)

    def assignment_statement(self) -> AssignmentStatement:
        # Assign the evaluated form of expr (or a full function since functions are their own eval) to the identifier
        identifier = self.identifier_reference()
        assignment_operator = self.eat("assignment_operator")
        if self.checkNextToken("keyword", "function_keyword"):
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

    def identifier_reference(self) -> IdentifierRefrence:
        # Get the value of the identifier
        identifier = self.eat("identifier")
        return IdentifierRefrence(
            identifier.value, identifier.line, identifier.col, self.program
        )

    def function_call(self, function: Function, identifier: str | None = None) -> FunctionCall:
        # Call a function
        start_tok = self.getNextToken()
        bracket_type = self.eat("open_bracket").value
        args = []
        if not self.checkNextToken("close_bracket", bracket_type):
            args = self.expression_list()
        try:
            self.eat("close_bracket", bracket_type)
        except:
            raise self.get_error(
                f"Expected a right bracket at the end of argument list, got {self.getNextToken().type}"
            )
        # If allows for calling function return values (ofc this will error if the function doesn't return another function)
        return FunctionCall(self.function_call(function) if self.checkNextToken("open_bracket") else function, args, identifier, start_tok.line, start_tok.col, self.program)

    def anonymous_function_or_call(self) -> Function | FunctionCall:
        # An anonymous function or anonymous function call. Can be an expression or a statement.
        kwd = self.eat("keyword", "function_keyword")
        args = self.identifier_name_list()
        body = self.block()
        if self.checkNextToken("open_bracket"):
            return self.function_call(Function(args, body, kwd.line, kwd.col, self.program))
        else:
            return Function(args, body, kwd.line, kwd.col, self.program)

    def literal_string(self) -> LiteralExpression:
        # Get a string literal
        token = self.eat("literal_string")
        return LiteralExpression(
            token.value, "string", token.line, token.col, self.program
        )

    def literal_integer(self) -> LiteralExpression:
        # Get an integer literal
        token = self.eat("literal_integer")
        return LiteralExpression(
            token.value, "integer", token.line, token.col, self.program
        )

    def literal_float(self) -> LiteralExpression:
        # Get a float literal
        token = self.eat("literal_float")
        return LiteralExpression(
            token.value, "float", token.line, token.col, self.program
        )

    def literal_boolean(self) -> LiteralExpression:
        # Get a boolean literal
        token = self.eat("literal_boolean")
        return LiteralExpression(
            token.value, "boolean", token.line, token.col, self.program
        )
        
    def array_literal(self) -> LiteralExpression:
        # Get an array literal
        # Even though it's called an array "literal", I don't group it in with the literal function because it can consist of identifiers and such
        self.eat("misc_symbol", "array_start")
        elements = []
        if not self.checkNextToken("misc_symbol", "array_end"):
            elements = self.expression_list()
        self.eat("misc_symbol", "array_end")
        return ArrayExpression(
            elements, self.getNextToken().line, self.getNextToken().col, self.program
        )

    def literal(self) -> LiteralExpression:
        # Get a generalized literal
        if self.checkNextToken("literal_string"):
            return self.literal_string()
        elif self.checkNextToken("literal_integer"):
            return self.literal_integer()
        elif self.checkNextToken("literal_float"):
            return self.literal_float()
        elif self.checkNextToken("literal_boolean"):
            return self.literal_boolean()
        else:
            raise self.get_error(f"Unknown literal type: {self.getNextToken().type}")

    def function(self) -> Function:
        # Get a function. Can be passed into a FunctionCall to use
        fn_token = self.eat("keyword", "function_keyword")
        args = self.identifier_name_list()
        body = self.block()
        return Function(args, body, fn_token.line, fn_token.col, self.program)

    def identifier_name_list(self) -> list[str]:
        # Get a list of identifier strings (used for function arguments)
        if not self.checkNextToken("identifier"):
            return []
        identifiers = [self.eat("identifier").value]
        while self.checkNextToken("misc_symbol", "comma"):
            self.eat()
            identifiers.append(self.eat("identifier").value)
        return identifiers

    def expression_list(self) -> list[Expression]:
        # Get a list of expressions (used for function call arguments)
        expressions = [self.expression()]
        while self.checkNextToken("misc_symbol", "comma"):
            self.eat()
            expressions.append(self.expression())
        return expressions


    # The WHOLE expression saga
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

    def primitive(
        self,
    ) -> IdentifierRefrence | LiteralExpression | FunctionCall | Function:
        # An expresion that can't be broken down any further
        if self.getNextToken().type == "identifier":
            if self.tokens[self.pos + 1].type == "open_bracket":
                return self.function_call(self.identifier_reference(), self.getNextToken().value)
            return self.identifier_reference()
        elif self.checkNextToken("keyword", "function_keyword"):
            return self.anonymous_function_or_call()
        elif self.checkNextToken("misc_symbol", "array_start"):
            return self.array_literal()
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
        # Get the precedence of a binary operator
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
        # Get the index of the (first) highest precedence operator in the list
        highest_precedence = 0
        idx = 0
        for i, op in enumerate(operators):
            if BINARY_OPERATOR_PRECEDENCE[op] > highest_precedence:
                highest_precedence = BINARY_OPERATOR_PRECEDENCE[op]
                idx = i
        return idx

    def merge_expression_transitions(self, expressions: list[Expression], operators: list[str]) -> list[Expression]:
        # Because higner-precedence operators are nested deeper, we need to put the deepest ones in first'
        while len(operators) > 0:
            idx = self.get_highest_precedence_index(operators)
            op = operators[idx]
            del operators[idx]
            # Merge the terms on either side of the operator
            expressions[idx] = BinaryExpression(expressions[idx], op, expressions.pop(idx + 1), expressions[idx].line, expressions[idx].col, self.program)
        return expressions[0]

    
    def print_ast(self):
        for statement in self.AST:
            print(statement.pretty_string())