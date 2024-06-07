WHITESPACES = [" ", "\t", "\n", "\r"]

COMMENTS = ["#"]

QUOTES = ["\"", "'", "`"]

# Reserved chars aren't allowed in symbols (the list implicitly includes all characters that are allowed in words)
RESERVED_CHARS = WHITESPACES + COMMENTS + QUOTES

# Symbols work similarly to keywords, but don't need spaces around them
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

ASSIGNMENT_OPERATORS = ["plus_equal", "minus_equal", "times_equal", "over_equal", "power_equal", "mod_equal", "and_equal", "or_equal", "assign"]

BINARY_OPERATORS = ["plus", "minus", "times", "over", "mod", "power", "and", "or", "equals", "not_equal", "lessthan", "lessthan_equal", "greaterthan", "greaterthan_equal"]

UNARY_OPERATORS = ["minus", "not"]

# Brackets are considered interchangeable. If you don't like it, split them out into groups and change the parser
OPEN_BRACKETS = ["open_paren", "open_bracket"]
CLOSE_BRACKETS = ["close_paren", "close_bracket"]

# Open brackets can only be closed by a closing bracket of the same type
BRACKET_TYPES = {
    "open_paren": "paren",
    "open_bracket": "bracket",
    "close_paren": "paren",
    "close_bracket": "bracket",
}

MISC_SYMBOLS = ["comma", "dot", "open_brace", "close_brace"]

# MAKE ALL OPERATOR TOKENS HAVE A VALUE OF THEIR INDEX IN THE RESPECTIVE LIST AND A TYPE OF THE KIND OF THE LIST

BINARY_OPERATOR_PRECEDENCE = {
    "plus": 1,
    "minus": 1,
    "times": 2,
    "over": 2,
    "mod": 2,
    "power": 3,
    "and": 0,
    "or": 0,
    "equals": 1,
    "not_equal": 1,
    "lessthan": 1,
    "lessthan_equal": 1,
    "greaterthan": 1,
    "greaterthan_equal": 1,
}

BINARY_OPERATIONS = {
    "plus": lambda a, b: a + b,
    "minus": lambda a, b: a - b,
    "times": lambda a, b: a * b,
    "over": lambda a, b: a / b,
    "mod": lambda a, b: a % b,
    "power": lambda a, b: a ** b,
    "and": lambda a, b: a and b,
    "or": lambda a, b: a or b,
    "equals": lambda a, b: a == b,
    "not_equal": lambda a, b: a != b,
    "lessthan": lambda a, b: a < b,
    "lessthan_equal": lambda a, b: a <= b,
    "greaterthan": lambda a, b: a > b,
    "greaterthan_equal": lambda a, b: a >= b,
}

UNARY_OPERATIONS = {
    "minus": lambda a: -a,
    "not": lambda a: not a,
}

ASSIGNMENT_OPERATIONS = {
    "assign": None,
    "plus_equal": lambda a, b: a + b,
    "minus_equal": lambda a, b: a - b,
    "times_equal": lambda a, b: a * b,
    "over_equal": lambda a, b: a / b,
    "mod_equal": lambda a, b: a % b,
    "power_equal": lambda a, b: a ** b,
    "and_equal": lambda a, b: a and b,
    "or_equal": lambda a, b: a or b,
}


UNARY_OPERATOR_PRECEDENCE = {
    "minus": 0,
    "not": 0,
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

class ImplementationError(LanguageError):
    pass

class ConfigError(Exception):
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


