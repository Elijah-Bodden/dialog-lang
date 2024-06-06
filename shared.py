WHITESPACES = [" ", "\t", "\n", "\r"]

COMMENTS = ["#"]

QUOTES = ["\"", "'", "`"]

# TODO: implement multi character operators
# TODO: add unary operators

BINARY_OPERATORS = {
    "+": "plus",
    "-": "minus",
    "*": "times",
    "/": "over",
    "=": "equals",
    "<": "lessthan",
    ">": "greaterthan",
    "&": "and",
    "|": "or",
    "^": "power",
    "%": "mod",
}

BINARY_OPERATOR_PRECEDENCE = {
    "plus": 0,
    "minus": 0,
    "times": 1,
    "over": 1,
    "equals": 0,
    "lessthan": 0,
    "greaterthan": 0,
    "not": 1,
    "and": 1,
    "or": 1,
    "power": 2,
    "mod": 1,
}

# Characters that can be used in identifiers and keywords (in addition to alphanumerics)
# I call both of these words
OK_IN_WORD = ["_"]

# All unary operators have the same precedence
UNARY_OPERATORS = {
    "-": "minus",
    "!": "not",
}



NON_OPERATOR_SYMBOLS = {
    ".": "dot",
    ",": "comma",
    ":": "assign",
    "(": "open_paren",
    ")": "close_paren",
    "[": "open_bracket",
    "]": "close_bracket",
    "{": "open_brace",
    "}": "close_brace",
}

KEYWORDS = {
    "if": "keyword_if",
    "else": "keyword_else",
    "elif": "keyword_elif",
    "while": "keyword_while",
    "for": "keyword_for",
    "return": "keyword_return",
    "print": "keyword_print",
    "then": "keyword_then",
    "error": "keyword_error",
}

# Start with truthy one
BOOLS = ["true", "false"]


OPERATIONS = {
    "plus": lambda a, b: a + b,
    "minus": lambda a, b: a - b,
    "times": lambda a, b: a * b,
    "over": lambda a, b: a * 1/ b,
    "equals": lambda a, b: a == b,
    "lessthan": lambda a, b: a < b,
    "greaterthan": lambda a, b: a > b,
    "not": lambda a: not a,
    "and": lambda a, b: a and b,
    "or": lambda a, b: a or b,
    "power": lambda a, b: a ** b,
    "mod": lambda a, b: a % b,
}


class LanguageError(Exception):
    def __init__(self, message, line, col, program):
        self.line = line
        self.col = col
        self.program = program
        self.message = message

    def __str__(self):
        lines = self.program.split("\n")
        return f"{self.message} at line {self.line}, col {self.col}\n>>> {lines[self.line]}\n{' ' * (self.col + 3) + '^'}"

class LexerError(LanguageError):
    pass

class ParserError(LanguageError):
    pass

class Token:
    def __init__(self, type, value, line, col):
        self.type = type
        self.value = value
        self.line = line
        self.col = col

    def __str__(self):
        return f"{self.type} {self.value}"
    
    def __repr__(self):
        return self.__str__()
