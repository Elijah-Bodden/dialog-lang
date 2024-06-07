from shared import *


class Lexer:
    def __init__(self, program: str):
        self.program = program
        self.pos = 0
        self.line = 0
        self.col = 0

    def current_char(self) -> str:
        return self.program[self.pos]

    def peekChar(self) -> str | None:
        return self.program[self.pos + 1] if self.pos + 1 < len(self.program) else None

    def peekDigit(self) -> bool:
        return self.peekChar().isdigit() if self.peekChar() else False

    def checkEOS(self) -> bool:
        return self.pos >= len(self.program)

    def eat_char(self, chars: list[str] = []) -> str:
        char = self.current_char()
        if chars and char not in chars:
            raise self.getError(
                f"Unexpected character. Expected one of: {str(chars)} but got {char}"
            )
        self.pos += 1
        self.col += 1
        if char == "\n":
            self.line += 1
            self.col = 0
        return char

    def charIsOkForWord(self, char: str) -> bool:
        return char.isalnum() or char in OK_IN_WORD

    def getError(self, message: str) -> LexerError:
        return LexerError(message, self.line, self.col, self.program)

    def getImplementationError(self, message: str) -> ImplementationError:
        return ImplementationError(
            f"{message} This is unexpected and probably a bug in the lexer.",
            self.line,
            self.col,
            self.program,
        )

    def comment(self):
        # Eat the stuff after the comment mark
        while not self.checkEOS() and self.current_char() != "\n":
            self.eat_char()

    def unescapeChar(self):
        try:
            self.eat_char(["\\"])
        except LexerError:
            raise self.getImplementationError(
                "Expected escape sequence but didn't get one."
            )
        char = self.eat_char()
        if char == "n":
            return "\n"
        elif char == "t":
            return "\t"
        elif char == "r":
            return "\r"
        elif char == "\\":
            return "\\"
        elif char == "'":
            return "'"
        elif char == '"':
            return '"'
        elif char == "`":
            return "`"
        else:
            raise self.getError(f"Unknown escape sequence: \\{char}")

    def getString(self, delimiter: str) -> str:
        string = ""
        try:
            self.eat_char([delimiter])
        except LexerError:
            raise self.getImplementationError(
                "Expected string start delimiter but didn't get one."
            )
        while not self.checkEOS() and self.current_char() != delimiter:
            if self.current_char() == "\n":
                raise self.getError("Line break before the end of string literal")
            if self.current_char() == "\\":
                string += self.unescapeChar()
            else:
                string += self.eat_char()
        try:
            self.eat_char([delimiter])
        except IndexError:
            raise self.getError("Unterminated string")
        return string

    def skipWhitespace(self):
        while not self.checkEOS() and self.current_char() in WHITESPACES:
            self.eat_char(WHITESPACES)

    def getNumber(self):
        number = ""
        try:
            number = self.eat_char(["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])
        except LexerError:
            raise self.getImplementationError("Expected number but didn't get one.")
        encountered_dot = False
        # While there's a digit OR a dot followed by a digit
        while not self.checkEOS() and (
            self.current_char().isdigit()
            or (self.current_char() == "." and not encountered_dot and self.peekDigit())
        ):
            if self.current_char() == ".":
                encountered_dot = True
                number += self.eat_char(["."])
            else:
                number += self.eat_char(
                    ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
                )
        return Token("literal_number", float(number), self.line, self.col)

    def getWord(self):
        word = ""
        if not self.charIsOkForWord(self.current_char()):
            raise self.getImplementationError("Expected word but didn't get one.")
        word += self.eat_char()
        while not self.checkEOS() and self.charIsOkForWord(self.current_char()):
            word += self.eat_char()
        return word

    def symbolThatStartsWith(self, string: str) -> str | None:
        # Return the symbol that starts with the given string
        for symbol in SYMBOLS:
            if symbol.startswith(string):
                return symbol
        return None

    def symbolExists(self, string: str) -> bool:
        # Return true if the given string is a symbol
        for symbol in SYMBOLS:
            if symbol == string:
                return True
        return False

    def charIsOkForSymbol(self, char: str) -> bool:
        return not self.charIsOkForWord(char) and char not in RESERVED_CHARS

    def getLongest(self, list: list[str]) -> str:
        longest = None
        for item in list:
            if longest is None or len(item) > len(longest):
                longest = item
        return longest

    def getSymbol(self) -> str:
        symbol = ""
        symbols = []
        while (
            not self.checkEOS()
            and self.symbolThatStartsWith(symbol + self.current_char())
            and self.charIsOkForSymbol(self.current_char())
        ):
            symbol += self.eat_char()
            if self.symbolExists(symbol):
                symbols.append(symbol)
        if len(symbols) == 0:
            raise self.getError(
                f"Expected symbol but didn't get one ('{symbol}' not in SYMBOLS)"
            )
        return self.getLongest(symbols)

    def getSymbolToken(self) -> Token:
        symbol = SYMBOLS[self.getSymbol()]
        if symbol in ASSIGNMENT_OPERATORS:
            return Token("assignment_operator", symbol, self.line, self.col)
        elif symbol in BINARY_OPERATORS:
            if symbol in UNARY_OPERATORS:
                return Token("ambiguous_operator", symbol, self.line, self.col)
            return Token("binary_operator", symbol, self.line, self.col)
        elif symbol in UNARY_OPERATORS:
            return Token("unary_operator", symbol, self.line, self.col)
        elif symbol in OPEN_BRACKETS:
            return Token("open_bracket", BRACKET_TYPES[symbol], self.line, self.col)
        elif symbol in CLOSE_BRACKETS:
            return Token("close_bracket", BRACKET_TYPES[symbol], self.line, self.col)
        elif symbol in MISC_SYMBOLS:
            return Token("misc_symbol", symbol, self.line, self.col)
        else:
            raise self.getError(
                f"Symbol not in any of the known types (assignment_operator, binary_operator, unary_operator, open_bracket, close_bracket, misc_symbol): {symbol}. Add the symbol to one of those lists or modify getSymbolToken to allow its type"
            )

    def getNextToken(self) -> Token:
        if self.checkEOS():
            return Token("EOS", "", self.line, self.col)

        current_char = self.current_char()

        if current_char in COMMENTS:
            self.comment()
            return self.getNextToken()

        elif current_char in QUOTES:
            string = self.getString(current_char)
            return Token("literal_string", string, self.line, self.col)

        elif current_char in WHITESPACES:
            # Ignore whitespaces
            self.skipWhitespace()
            return self.getNextToken()

        elif current_char.isdigit():
            return self.getNumber()

        elif self.charIsOkForSymbol(current_char):
            # If char IS ok for symbol, it's NOT ok for word, so we can assume it's a symbol
            return self.getSymbolToken()

        else:
            if not self.charIsOkForWord(current_char):
                raise self.getError(
                    f"Unexpected character. Expected a word but got {current_char}"
                )
            lexeme = self.getWord()

            if lexeme in BOOLS:
                return Token("literal_boolean", lexeme == BOOLS[0], self.line, self.col)
            elif lexeme in KEYWORDS.keys():
                return Token(KEYWORDS[lexeme], lexeme, self.line, self.col)
            else:
                return Token("identifier", lexeme, self.line, self.col)

    def getArrDiff(
        self, arr1: list[str], arr2: list[str]
    ) -> tuple[list[str], list[str]]:
        not_in_arr1 = []
        for item in arr2:
            if item not in arr1:
                not_in_arr1.append(item)
        not_in_arr2 = []
        for item in arr1:
            if item not in arr2:
                not_in_arr2.append(item)
        return (not_in_arr1, not_in_arr2)

    def listEqual(self, list1: list[str], list2: list[str]) -> bool:
        diff = self.getArrDiff(list1, list2)
        return len(diff[0]) == 0 and len(diff[1]) == 0

    def preCheck(self):
        for symbol in SYMBOLS:
            for char in symbol:
                if not self.charIsOkForSymbol(char):
                    raise ConfigError(
                        f"CONFIG ERROR: Character '{char}' is not allowed in symbols, but occurs in '{symbol}' in the SYMBOLS list."
                    )
        for keyword in KEYWORDS.keys():
            for char in keyword:
                if not self.charIsOkForWord(char):
                    raise ConfigError(
                        f"CONFIG ERROR: Character '{char}' is not allowed in keywords, but occurs in '{keyword}' in the KEYWORDS list."
                    )
        if len(BOOLS) != 2:
            raise ConfigError("CONFIG ERROR: BOOLS list must have two elements.")
        if not self.listEqual(BINARY_OPERATORS, BINARY_OPERATOR_PRECEDENCE.keys()):
            diff = self.getArrDiff(BINARY_OPERATORS, BINARY_OPERATOR_PRECEDENCE.keys())
            raise ConfigError(
                f"CONFIG ERROR: BINARY_OPERATOR_PRECEDENCE's keys must match BINARY_OPERATORS (BINARY_OPERATORS is missing {diff[0]}, BINARY_OPERATOR_PRECEDENCE is missing {diff[1]})."
            )
        if not self.listEqual(UNARY_OPERATORS, UNARY_OPERATOR_PRECEDENCE.keys()):
            diff = self.getArrDiff(UNARY_OPERATORS, UNARY_OPERATOR_PRECEDENCE.keys())
            raise ConfigError(
                f"CONFIG ERROR: UNARY_OPERATOR_PRECEDENCE's keys must match UNARY_OPERATORS (UNARY_OPERATORS is missing {diff[0]}, UNARY_OPERATOR_PRECEDENCE is missing {diff[1]})."
            )
        if not self.listEqual(ASSIGNMENT_OPERATORS, ASSIGNMENT_OPERATIONS.keys()):
            diff = self.getArrDiff(ASSIGNMENT_OPERATORS, ASSIGNMENT_OPERATIONS.keys())
            raise ConfigError(
                f"CONFIG ERROR: ASSIGNMENT_OPERATIONS's keys must match ASSIGNMENT_OPERATORS (ASSIGNMENT_OPERATORS is missing {diff[0]}, ASSIGNMENT_OPERATIONS is missing {diff[1]})."
            )
        if not self.listEqual(BINARY_OPERATORS, BINARY_OPERATIONS.keys()):
            diff = self.getArrDiff(BINARY_OPERATORS, BINARY_OPERATIONS.keys())
            raise ConfigError(
                f"CONFIG ERROR: BINARY_OPERATIONS's keys must match BINARY_OPERATORS (BINARY_OPERATORS is missing {diff[0]}, BINARY_OPERATIONS is missing {diff[1]})."
            )
        if not self.listEqual(UNARY_OPERATORS, UNARY_OPERATIONS.keys()):
            diff = self.getArrDiff(UNARY_OPERATORS, UNARY_OPERATIONS.keys())
            raise ConfigError(
                f"CONFIG ERROR: UNARY_OPERATIONS's keys must match UNARY_OPERATORS (UNARY_OPERATORS is missing {diff[0]}, UNARY_OPERATIONS is missing {diff[1]})."
            )

    def tokenize(self) -> list[Token]:
        self.preCheck()
        tokens = []
        # The not tokens is to make sure we don't index an empty list
        while not tokens or tokens[-1].type != "EOS":
            token = self.getNextToken()
            tokens.append(token)
        return tokens
