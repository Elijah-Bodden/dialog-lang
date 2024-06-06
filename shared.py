WHITESPACES = [" ", "\t", "\n", "\r"]

COMMENTS = ["#"]

QUOTES = ["\"", "'", "`"]

SYMBOLS = {
    ".": "dot",
    ",": "comma",
    ":": "assign",
    "(": "open_paren",
    ")": "close_paren",
    "[": "open_bracket",
    "]": "close_bracket",
    "{": "open_brace",
    "}": "close_brace",
    "+": "plus",
    "+=": "plus_equal",
    "-": "minus",
    "-=": "minus_equal",
    "*": "times",
    "*=": "times_equal",
    "/": "over",
    "/=": "over_equal",
    "==": "equals",
    "<": "lessthan",
    "<=": "lessthan_equal",
    ">": "greaterthan",
    ">=": "greaterthan_equal",
    "!": "not",
    "!=": "not_equal",
    "&&": "and",
    "&=": "and_equal",
    "||": "or",
    "|=": "or_equal",
    "^": "power",
    "^=": "power_equal",
    "%": "mod",
    "%=": "mod_equal",
}

ASSIGNMENT_OPERATORS = ["=", "+=", "-=", "*=", "/=", "^=", "%=", "&=", "|=", ":"]

BINARY_OPERATORS = ["+", "-", "*", "/", "^", "%", "&&", "||", "==", "!=", "<", "<=", ">", ">="]

UNARY_OPERATORS = ["-", "!"]

BRACKETS = ["(", ")", "[", "]", "{", "}"]

MISC_SYMBOLS = [",", "."]



BINARY_OPERATOR_PRECEDENCE = {
    "plus": 0,
    "minus": 0,
    "times": 1,
    "over": 1,
    "mod": 1,
    "power": 2,
    "and": 1,
    "or": 1,
    "equals": 0,
    "not_equal": 0,
    "lessthan": 0,
    "lessthan_equal": 0,
    "greaterthan": 0,
    "greaterthan_equal": 0,
}

BINARY_OPERATIONS = {
    "+": lambda a, b: a + b,
    "-": lambda a, b: a - b,
    "*": lambda a, b: a * b,
    "/": lambda a, b: a / b,
    "^": lambda a, b: a ** b,
    "%": lambda a, b: a % b,
    "&&": lambda a, b: a and b,
    "||": lambda a, b: a or b,
    "==": lambda a, b: a == b,
    "!=": lambda a, b: a != b,
    "<": lambda a, b: a < b,
    "<=": lambda a, b: a <= b,
    ">": lambda a, b: a > b,
    ">=": lambda a, b: a >= b,
}

UNARY_OPERATIONS = {
    "-": lambda a: a,
    "!": lambda a: not a,
}

ASSIGNMENT_OPERATIONS = {
    "=": lambda a, b: b,
    "+=": lambda a, b: a + b,
    "-=": lambda a, b: a - b,
    "*=": lambda a, b: a * b,
    "/=": lambda a, b: a / b,
    "^=": lambda a, b: a ** b,
    "%=": lambda a, b: a % b,
    "&=": lambda a, b: a and b,
    "|=": lambda a, b: a or b,
}


UNARY_OPERATOR_PRECEDENCE = {
    "-": 0,
    "!": 0,
}


# Characters that can be used in identifiers and keywords (in addition to alphanumerics)
# I call both of these words
OK_IN_WORD = ["_"]


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
    "fn": "keyword_function",
}

# Start with truthy one
BOOLS = ["true", "false"]


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

class ImplementationError(Exception):
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


