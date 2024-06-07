from shared import *

# Merge symbolic operators in the lexer
# TODO: Add types
# TODO: add some more non-standard literals for fun
# TODO: maybe add syntax highlighting
# TODO: allow function calls on expressions (treat them as their own objects)

# TODO: ambiguous operators are are binary operators if allowed by the grammar, else unary operators. Handle those in parser

class Lexer:
    def __init__(self, program):
        self.program = program
        self.pos = 0
        self.line = 0
        self.col = 0

    def current_char(self):
        return self.program[self.pos]
    
    def peekChar(self):
        return self.program[self.pos + 1] if self.pos + 1 < len(self.program) else None

    def peekDigit(self):
        return self.peekChar().isdigit() if self.peekChar() else False
    
    def checkEOS(self):
        return self.pos >= len(self.program)

    def eat_char(self, chars=[]):
        char = self.current_char()
        if chars and char not in chars:
            raise self.getError(f"Unexpected character. Expected one of: {str(chars)} but got {char}")
        self.pos += 1
        self.col += 1
        if char == "\n":
            self.line += 1
            self.col = 0
        return char
    
    def charIsOkForWord(self, char):
        return char.isalnum() or char in OK_IN_WORD

    def getError(self, message):
        return LexerError(message, self.line, self.col, self.program)
    
    def getImplementationError(self, message):
        return ImplementationError(f"{message} This is unexpected and probably a bug in the lexer.", self.line, self.col, self.program)

    def comment(self):
        # Eat the stuff after the comment mark
        while not self.checkEOS() and self.current_char() != "\n":
            self.eat_char()

    def unescapeChar(self):
        try:
            self.eat_char(["\\"])
        except LexerError:
            raise self.getImplementationError("Expected escape sequence but didn't get one.")
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

    def getString(self, delimiter):
        string = ""
        try:
            self.eat_char([delimiter])
        except LexerError:
            raise self.getImplementationError("Expected string start delimiter but didn't get one.")
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
                number += self.eat_char(["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])
        return Token("literal_number", float(number), self.line, self.col)

    def getWord(self):
        word = ""
        if not self.charIsOkForWord(self.current_char()):
            raise self.getImplementationError("Expected word but didn't get one.")
        word += self.eat_char()
        while not self.checkEOS() and self.charIsOkForWord(self.current_char()):
            word += self.eat_char()
        return word
    
    def symbolStartsWith(self, string):
        # Return the symbol that starts with the given string
        for symbol in SYMBOLS:
            if symbol.startswith(string):
                return symbol
        return None
    
    def symbolExists(self, string):
        # Return true if the given string is a symbol
        for symbol in SYMBOLS:
            if symbol == string:
                return True
        return False
    
    def charIsOkForSymbol(self, char):
        return not self.charIsOkForWord(char) and char not in RESERVED_CHARS

    def getLongest(self, list):
        longest = None
        for item in list:
            if longest is None or len(item) > len(longest):
                longest = item
        return longest

    def getSymbol(self):
        # Get the raw string for the next symbol
        symbol = ""
        symbols = []
        while not self.checkEOS() and self.symbolStartsWith(symbol + self.current_char()) and self.charIsOkForSymbol(self.current_char()):
            symbol += self.eat_char()
            if self.symbolExists(symbol):
                symbols.append(symbol)
        if len(symbols) == 0:
            raise self.getError(f"Expected symbol but didn't get one ('{symbol}' not in SYMBOLS)")
        return self.getLongest(symbols)
    
    def getSymbolToken(self):
        # Get the next symbol, with the type of the symbol
        symbol = self.getSymbol()
        if symbol in ASSIGNMENT_OPERATORS:
            return Token("assignment_operator", SYMBOLS[symbol], self.line, self.col)
        elif symbol in BINARY_OPERATORS:
            if symbol in UNARY_OPERATORS:
                return Token("ambiguous_operator", SYMBOLS[symbol], self.line, self.col)
            return Token("binary_operator", SYMBOLS[symbol], self.line, self.col)
        elif symbol in UNARY_OPERATORS:
            return Token("unary_operator", SYMBOLS[symbol], self.line, self.col)
        elif symbol in BRACKETS:
            return Token("bracket", SYMBOLS[symbol], self.line, self.col)
        elif symbol in MISC_SYMBOLS:
            return Token("misc_symbol", SYMBOLS[symbol], self.line, self.col)
        else:
            raise self.getError(f"Symbol not in any of the known types (assignment_operator, binary_operator, unary_operator, bracket, misc_symbol): {symbol}. Add the symbol to one of those lists or modify getSymbolToken to allow its type")


    def getNextToken(self):
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
                raise self.getError(f"Unexpected character. Expected a word but got {current_char}")
            lexeme = self.getWord()

            if lexeme in BOOLS:
                return Token("literal_boolean", lexeme == BOOLS[0], self.line, self.col)
            elif lexeme in KEYWORDS.keys():
                return Token(KEYWORDS[lexeme], lexeme, self.line, self.col)
            else:
                return Token("identifier", lexeme, self.line, self.col)

    def preCheck(self):
        for symbol in SYMBOLS:
            for char in symbol:
                if not self.charIsOkForSymbol(char):
                    raise ConfigError(f"CONFIG ERROR: Character '{char}' is not allowed in symbols, but occurs in '{symbol}' in the SYMBOLS list.")
        for keyword in KEYWORDS.keys():
            for char in keyword:
                if not self.charIsOkForWord(char):
                    raise ConfigError(f"CONFIG ERROR: Character '{char}' is not allowed in keywords, but occurs in '{keyword}' in the KEYWORDS list.")
        if len(BOOLS) != 2:
            raise ConfigError("CONFIG ERROR: BOOLS list must have two elements.")
        if len(BINARY_OPERATORS) != len(BINARY_OPERATOR_PRECEDENCE):
            raise ConfigError(f"CONFIG ERROR: BINARY_OPERATORS list must have the same length as BINARY_OPERATOR_PRECEDENCE (BINARY_OPERATORS has {len(BINARY_OPERATORS)} elements, BINARY_OPERATOR_PRECEDENCE has {len(BINARY_OPERATOR_PRECEDENCE)} elements).")
        if len(BINARY_OPERATORS) != len(BINARY_OPERATIONS):
            raise ConfigError(f"CONFIG ERROR: BINARY_OPERATORS list must have the same length as BINARY_OPERATIONS (BINARY_OPERATORS has {len(BINARY_OPERATORS)} elements, BINARY_OPERATIONS has {len(BINARY_OPERATIONS)} elements).")
        if len(UNARY_OPERATORS) != len(UNARY_OPERATOR_PRECEDENCE):
            raise ConfigError(f"CONFIG ERROR: UNARY_OPERATORS list must have the same length as UNARY_OPERATOR_PRECEDENCE (UNARY_OPERATORS has {len(UNARY_OPERATORS)} elements, UNARY_OPERATOR_PRECEDENCE has {len(UNARY_OPERATOR_PRECEDENCE)} elements).")
        if len(UNARY_OPERATORS) != len(UNARY_OPERATIONS):
            raise ConfigError(f"CONFIG ERROR: UNARY_OPERATORS list must have the same length as UNARY_OPERATIONS (UNARY_OPERATORS has {len(UNARY_OPERATORS)} elements, UNARY_OPERATIONS has {len(UNARY_OPERATIONS)} elements).")
        if len(ASSIGNMENT_OPERATORS) != len(ASSIGNMENT_OPERATIONS):
            raise ConfigError(f"CONFIG ERROR: ASSIGNMENT_OPERATORS list must have the same length as ASSIGNMENT_OPERATIONS (ASSIGNMENT_OPERATORS has {len(ASSIGNMENT_OPERATORS)} elements, ASSIGNMENT_OPERATIONS has {len(ASSIGNMENT_OPERATIONS)} elements).")

    def tokenize(self):
        self.preCheck()
        tokens = []
        # The not tokens is to make sure we don't index an empty list
        while not tokens or tokens[-1].type != "EOS":
            token = self.getNextToken()
            tokens.append(token)
        return tokens