from shared import *

# Merge symbolic operators in the lexer
# TODO: Add types
# TODO: add some more non-standard literals for fun

class Lexer:
    def __init__(self, program):
        self.program = program
        self.pos = 0
        self.line = 0
        self.col = 0

    def current_char(self):
        return self.program[self.pos]

    def eat_char(self):
        char = self.current_char()
        self.pos += 1
        self.col += 1
        if char == "\n":
            self.line += 1
            self.col = 0
        return char

    def peekChar(self):
        return self.program[self.pos + 1] if self.pos + 1 < len(self.program) else None

    def peekDigit(self):
        return self.peekChar().isdigit() if self.peekChar() else False

    def checkEOS(self):
        return self.pos >= len(self.program)

    def tokenize(self):
        tokens = []
        # The len is to make sure we don't index an empty list
        while len(tokens) < 1 or tokens[-1].type != "EOS":
            token = self.getNextToken()
            tokens.append(token)
        return tokens

    def comment(self):
        # Eat the stuff after the comment mark
        while not self.checkEOS() and self.current_char() != "\n":
            self.eat_char()

    def unescapeChar(self):
        self.eat_char()
        char = self.eat_char()
        if char == "n":
            return "\n"
        elif char == "t":
            return "\t"
        elif char == "r":
            return "\r"
        else:
            return "\\" + char


    def getString(self, delimiter):
        string = ""
        while not self.checkEOS() and self.current_char() != delimiter:
            if self.current_char() == "\n":
                raise LexerError(
                    "Line break in the middle of a string",
                    self.line,
                    self.col,
                    self.program,
                )
            if self.current_char() == "\\":
                string += self.unescapeChar()
            else:
                string += self.eat_char()
        try:
            self.eat_char()
        except IndexError:
            raise LexerError("Unterminated string", self.line, self.col, self.program)
        return string

    def skipWhitespace(self):
        while not self.checkEOS() and self.current_char() in WHITESPACES:
            self.eat_char()

    def getNumber(self, first_char):
        number = first_char
        encountered_dot = False
        # While there's a digit OR a dot followed by a digit
        while not self.checkEOS() and (
            self.current_char().isdigit()
            or (self.current_char() == "." and not encountered_dot and self.peekDigit())
        ):
            if self.current_char() == ".":
                encountered_dot = True
            number += self.eat_char()
        return Token("literal_number", float(number), self.line, self.col)

    def getWord(self, first_char):
        word = first_char
        while not self.checkEOS() and self.charIsOkForWord(self.current_char()):
            word += self.eat_char()
        return word

    def charIsOkForWord(self, char):
        return char.isalnum() or char in OK_IN_WORD

    def getNextToken(self):
        if self.checkEOS():
            return Token("EOS", "", self.line, self.col)

        next_char = self.eat_char()

        if next_char in COMMENTS:
            self.comment()
            return self.getNextToken()

        elif next_char in QUOTES:
            string = self.getString(next_char)
            return Token("literal_string", string, self.line, self.col)

        elif next_char in WHITESPACES:
            # Ignore whitespaces
            self.skipWhitespace()
            return self.getNextToken()

        elif next_char.isdigit():
            return self.getNumber(next_char)

        elif next_char in BINARY_OPERATORS.keys():
            return Token(
                "binary_operator", BINARY_OPERATORS[next_char], self.line, self.col
            )

        elif next_char in NON_OPERATOR_SYMBOLS.keys():
            return Token(
                NON_OPERATOR_SYMBOLS[next_char], next_char, self.line, self.col
            )

        else:
            if not self.charIsOkForWord(next_char):
                raise LexerError(
                    "Unexpected character", self.line, self.col, self.program
                )
            lexeme = self.getWord(next_char)

            if lexeme in BOOLS:
                return Token("literal_boolean", lexeme == BOOLS[0], self.line, self.col)
            elif lexeme in KEYWORDS.keys():
                return Token(KEYWORDS[lexeme], lexeme, self.line, self.col)
            else:
                return Token("identifier", lexeme, self.line, self.col)
