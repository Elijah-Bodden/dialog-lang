# NEVER ALLOW TYPE COERCION
# REMEMBER WHEN YOU ADD A NEW TYPE OR OPERATOR, UPDATE THE COMPATIBILITY THINGIES
# Redo the way i implemented functions to be less retarded and more like expressions

def indent(string: str):
    lines = string.split("\n")
    lines = [f"{' ' * 2}{line}" for line in lines]
    return "\n".join(lines)

class Expression:
    def __init__(self, type: str, line: int, col: int, program: str) -> None:
        self.type = type
        self.line = line
        self.col = col
        self.program = program

    def __str__(self):
        return f"{self.type}"

    def __repr__(self):
        return self.__str__()

class EvaluatedExpression(Expression):
    def __init__(self, value, type: str, line: int, col: int, program: str) -> None:
        self.line = line
        self.col = col
        self.program = program
        # TODO: Remove the bypass stuff, it's just a temporary fix until i implement function returns.
        if type != "BYPASS" and type not in LANGUAGE_TYPES:
            raise ImplementationError(f"Unknown type: {type}", line, col, program)
        # Lmao functions get to skip type checks. This is probably bad.
        elif False if type=="function" or type=="BYPASS" else (not LANGUAGE_TYPE_CHECKS[type](value)):
            raise ImplementationError(f"Evaluated expression is not of type {type}: {value}", line, col, program)
        self.value = value
        self.type = type
        super().__init__(type, line, col, program)

class TypeExpression(Expression):
    def __init__(self, type: str, line: int, col: int, program: str) -> None:
        super().__init__("TypeExpression", line, col, program)
        self.value = type

    def eval(self, env: dict[str, Expression]):
        return self

    def __str__(self):
        return f"{self.type} {self.value}"

    def __repr__(self):
        return self.__str__()
    
    def pretty_string(self):
        return indent(self.__str__())

WHITESPACES = [" ", "\t", "\n", "\r"]

COMMENTS = ["#"]

QUOTES = ['"', "'", "`"]

# Reserved chars aren't allowed in symbols (the list implicitly includes all characters that are allowed in words)
RESERVED_CHARS = WHITESPACES + COMMENTS + QUOTES

# Characters that can be used in words (in addition to alphanumerics)
OK_IN_WORD = ["_"]

# Symbols and reserved words can be used interchangeably internally (they both get turned into internal names), the only difference is that symbols don't need spaces around them
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
RESERVED_WORDS = {
    "as": "as",
    "number": "number",
    "string": "string",
    "boolean": "boolean",
    "function": "function",
    "true": "true",
    "false": "false",
    "if": "if",
    "else": "else",
    "elif": "elif",
    "while": "while",
    "for": "for",
    "return": "return",
    "print": "print",
    "then": "then",
    "error": "error",
    "fn": "function_keyword",
    "typeof": "typeof",
}

ASSIGNMENT_OPERATORS = [
    "plus_equal",
    "minus_equal",
    "times_equal",
    "over_equal",
    "power_equal",
    "mod_equal",
    "and_equal",
    "or_equal",
    "assign",
]

BINARY_OPERATORS = [
    "plus",
    "minus",
    "times",
    "over",
    "mod",
    "power",
    "and",
    "or",
    "equals",
    "not_equal",
    "lessthan",
    "lessthan_equal",
    "greaterthan",
    "greaterthan_equal",
    "as"
]

UNARY_OPERATORS = [
    "minus",
    "not",
    "typeof",
]

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

LANGUAGE_TYPES = ["number", "string", "boolean", "function"]


# Start with truthy one
BOOLS = ["true", "false"]

KEYWORDS = [
    "if",
    "else",
    "elif",
    "while",
    "for",
    "return",
    "print",
    "then",
    "error",
    "function_keyword",
]



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
    "as": 0
}

BINARY_OPERATIONS = {
    "plus": lambda a, b: (a.value + b.value, a.type),
    "minus": lambda a, b: (a.value - b.value, a.type),
    "times": lambda a, b: (a.value * b.value, a.type),
    "over": lambda a, b: (a.value / b.value, a.type),
    "mod": lambda a, b: (a.value % b.value, a.type),
    "power": lambda a, b: (a.value**b.value, a.type),
    "and": lambda a, b: (a.value and b.value, "boolean"),
    "minus": lambda a, b: (a.value - b.value, a.type),
    "or": lambda a, b: (a.value or b.value, "boolean"),
    "equals": lambda a, b: (a.value == b.value, "boolean"),
    "not_equal": lambda a, b: (a.value != b.value, "boolean"),
    "lessthan": lambda a, b: (a.value < b.value, "boolean"),
    "lessthan_equal": lambda a, b: (a.value <= b.value, "boolean"),
    "greaterthan": lambda a, b: (a.value > b.value, "boolean"),
    "greaterthan_equal": lambda a, b: (a.value >= b.value, "boolean"),
    # This is just here for symmetry (lexer pre-checks), but it should never be called bc binary expression parser shortcuts to as_type
    "as": lambda a, b: None,
}

BINARY_OPERATOR_TYPE_SUPPORT = {
    "plus": ["number", "string"],
    "minus": ["number", "string"],
    "times": ["number", "string"],
    "over": ["number", "string"],
    "mod": ["number", "string"],
    "power": ["number", "string"],
    "and": ["boolean", "number", "string"],
    "or": ["boolean", "number", "string"],
    "equals": ["number", "string", "boolean"],
    "not_equal": ["number", "string", "boolean"],
    "lessthan": ["number", "string", "boolean"],
    "lessthan_equal": ["number", "string", "boolean"],
    "greaterthan": ["number", "string", "boolean"],
    "greaterthan_equal": ["number", "string", "boolean"],
    "as": ["number", "string", "boolean"]
}

def as_type(evaluated_expr: EvaluatedExpression, end_type: str) -> EvaluatedExpression:
    start_type = evaluated_expr.type
    if not end_type in LANGUAGE_TYPE_CONVERSIONS[start_type]:
        raise LanguageError(f"Cannot convert {start_type} to {end_type}", evaluated_expr.line, evaluated_expr.col, evaluated_expr.program)
    return EvaluatedExpression(LANGUAGE_TYPE_CONVERSIONS[start_type][end_type](evaluated_expr.value), end_type, evaluated_expr.line, evaluated_expr.col, evaluated_expr.program)

# Expressions only get a type after they've been evaluated
LANGUAGE_TYPE_CONVERSIONS = {
    # From type to type
    "number": {
        "number": lambda x: x,
        "string": lambda x: str(x),
        "boolean": lambda x: x != 0,
    },
    "string": {
        "string": lambda x: x,
        "boolean": lambda x: x != "",
    },
    "boolean": {
        "number": lambda x: 0 if x == 0 else 1,
        "string": lambda x: "true" if x else "false",
        "boolean": lambda x: x,
    },
    "function": {
    }
}
LANGUAGE_TYPE_CHECKS = {
    "number": lambda x: isinstance(x, float) or isinstance(x, int),
    "string": lambda x: isinstance(x, str),
    "boolean": lambda x: isinstance(x, bool),
    "function": None
}    


UNARY_OPERATIONS = {
    "minus": lambda a: (-a.value, a.type),
    "not": lambda a: (not a.value, a.type),
    "typeof": lambda a: (a.type, "string"),
}

UNARY_OPERATOR_TYPE_SUPPORT = {
    "minus": ["number"],
    "not": ["boolean"],
    "typeof": LANGUAGE_TYPES,
}


ASSIGNMENT_OPERATIONS = {
    # Also just for symmetry, gets skipped since you don't need to call a function to plain assign
    "assign": None,
    "plus_equal": lambda a, b: (a.value + b.value, "number"),
    "minus_equal": lambda a, b: (a.value - b.value, "number"),
    "times_equal": lambda a, b: (a.value * b.value, "number"),
    "over_equal": lambda a, b: (a.value / b.value, "number"),
    "mod_equal": lambda a, b: (a.value % b.value, "number"),
    "power_equal": lambda a, b: (a.value**b.value, "number"),
    "and_equal": lambda a, b: (a.value and b.value, "boolean"),
    "or_equal": lambda a, b: (a.value or b.value, "boolean"),
}
ASSIGNMENT_OPERATOR_TYPE_SUPPORT = {
    "assign": ["number", "string", "boolean", "function"],
    "plus_equal": ["number", "string", "boolean"],
    "minus_equal": ["number"],
    "times_equal": ["number"],
    "over_equal": ["number"],
    "mod_equal": ["number"],
    "power_equal": ["number"],
    "and_equal": ["boolean"],
    "or_equal": ["boolean"],
}


UNARY_OPERATOR_PRECEDENCE = {
    "minus": 0,
    "not": 0,
    "typeof": 0,
}


class LanguageError(Exception):
    def __init__(self, message, line, col, program):
        self.line = line
        self.col = col
        self.program = program
        self.message = message

    def __str__(self):
        lines = self.program.split("\n")
        return f"{self.message} at line {self.line + 1}, col {self.col}\n>>> {lines[self.line]}\n{' ' * (self.col + 3) + '^'}"


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
