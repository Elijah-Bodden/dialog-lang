"""
Terminology:
    - Word: A semantic unit made up of alphanumeric characters (and any other characters you choose to allow). Words must be broken up by whitespace or symbols
    - Symbol: A semantic unit that translates to an internal name and consists of non-word characters. Words and symbols both translate to internal names
    - Internal name: Identifies a unique token. Words and symbols both map to internal names, which in turn get turned into tokens
    You can have multiple symbols and/or words map to the same internal name
"""

from shared import LanguageError

# (Whitespaces basically)
IGNORE = (" ", "\t", "\n", "\r")
STRING_DELIMITERS = ('"', "'", "`")

# Used to determine what operations you can do between/to an expression
LANGUAGE_TYPES = ("integer", "float", "string", "boolean", "function", "array", "null")

# Characters that can't be used in symbols (the list implicitly includes all characters that are allowed in words)
RESERVED_CHARS = IGNORE + STRING_DELIMITERS 

# Characters that can be used in words (in addition to alphanumerics)
OK_IN_WORD = ("_",)

# Translates symbols to internal names
SYMBOLS = {
    ".": "dot",
    ",": "comma",
    ":": "assign",
    "(": "open_paren",
    ")": "close_paren",
    "[": "array_start",
    "]": "array_end",
    "{": "block_start",
    "}": "block_end",
    "+": "plus",
    "+=": "plus_equal",
    "-": "minus",
    "-=": "minus_equal",
    "*": "times",
    "*=": "times_equal",
    "/": "over",
    "//": "over_int",
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
    "@": "from",
    "#": "comment",
}

# Translates words to internal names
# Words in this list can't be used as identifiers (they're reserved)
RESERVED_WORDS = {
    "as": "as",
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
    "like": "stop_word",
    "umm": "stop_word",
    "um": "stop_word",
    "uh": "stop_word",
    "plus": "plus",
    "minus": "minus",
    "times": "times",
    "of": "times",
    "over": "over",
    "mod": "mod",
    "power": "power",
    "and": "and",
    "or": "or",
    "import": "import",
    "from": "from",
    "push": "push",
    "return": "return",
    "null": "null",
    "secret": "comment",
}

# Now we define the uses of different internal names

# Follow the pattern IDENTIFIER ASSIGNMENT_OPERATOR (any word or symbol that maps to it) EXPRESSION
# The identifier gets updated using the expression
ASSIGNMENT_OPERATORS = (
    "plus_equal",
    "minus_equal",
    "times_equal",
    "over_equal",
    "over_int_equal",
    "power_equal",
    "mod_equal",
    "and_equal",
    "or_equal",
    "assign",
    "push",
)


# The function that gets called on the values of the identifier and expression
# a is the value of the identifier before the assignment
# b is the value of the expression
# returns a tuple of the new value and the type of the new value
ASSIGNMENT_OPERATIONS = {
    "plus_equal": lambda a, b: (a.value + b.value, a.type),
    "minus_equal": lambda a, b: (a.value - b.value, a.type),
    "times_equal": lambda a, b: (a.value * b.value, a.type),
    "over_equal": lambda a, b: (a.value / b.value, a.type),
    "over_int_equal": lambda a, b: (a.value // b.value, a.type),
    "mod_equal": lambda a, b: (a.value % b.value, a.type),
    "power_equal": lambda a, b: (a.value**b.value, a.type),
    "and_equal": lambda a, b: (a.value and b.value, "boolean"),
    "or_equal": lambda a, b: (a.value or b.value, "boolean"),
    "push": lambda a, b: ([*a.value, b], "array"),
    # Just for symmetry (lexer pre-checks), but it should never be called bc assign is a special case (a doesn't need to be initialized)
    "assign": None,
}

# What internal types we support for each assignment operator
# A tuple of the identifier's start type and the expression's type
ASSIGNMENT_OPERATOR_TYPE_SUPPORT = {
    # We don't need to check compatibility for a plain assignment
    "assign": None,
    "plus_equal": [("integer", "integer"), ("float", "float"), ("string", "string"), ("array", "array")],
    "minus_equal": [("integer", "integer"), ("float", "float")],
    "times_equal": [("integer", "integer"), ("float", "float")],
    "over_equal": [("float", "float")],
    "over_int_equal": [("integer", "integer")],
    "mod_equal": [("integer", "integer"), ("float", "float")],
    "power_equal": [("integer", "integer"), ("float", "float")],
    "and_equal": [("boolean", "boolean")],
    "or_equal": [("boolean", "boolean")],
    "push": [("array", i) for i in LANGUAGE_TYPES]
}

# Same old deal, but for binary operators
# Binary operators have an expression on either side
# And create a new expression using the two expressions's values and an operaion
BINARY_OPERATORS = (
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
    "as",
    "from",
    "over_int",
)

# Functions used by config (for operations) AND nodes (internally)
def careful_from_array(index: int, array: list) -> tuple:
    try:
        return array[index].value, array[index].type
    except IndexError:
        raise LanguageError(f"Index {index} out of bounds for array of length {len(array)}", array[0].line, array[0].col, array[0].program)

def as_type(evaluated_expr, end_type: str):
    start_type = evaluated_expr.type
    if not end_type in LANGUAGE_TYPE_CONVERSIONS[start_type]:
        raise LanguageError(f"Cannot convert {start_type} to {end_type}", evaluated_expr.line, evaluated_expr.col, evaluated_expr.program)
    return (LANGUAGE_TYPE_CONVERSIONS[start_type][end_type](evaluated_expr.value), end_type)

BINARY_OPERATIONS = {
    "plus": lambda a, b: (a.value + b.value, a.type),
    "minus": lambda a, b: (a.value - b.value, a.type),
    "times": lambda a, b: (a.value * b.value, a.type),
    "over": lambda a, b: (a.value / b.value, a.type),
    "over_int": lambda a, b: (a.value // b.value, a.type),
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
    "from": lambda a, b: careful_from_array(a.value, b.value),
    "as": lambda a, b: as_type(a, b.value),
}

# Tuple of (left type, right type)
BINARY_OPERATOR_TYPE_SUPPORT = {
    "plus": [("float", "float"), ("integer", "integer"), ("string", "string"), ("array", "array")],
    "minus": [("float", "float"), ("integer", "integer")],
    "times": [("float", "float"), ("integer", "integer")],
    "over": [("float", "float")],
    "over_int": [("integer", "integer")],
    "mod": [("float", "float"), ("integer", "integer")],
    "power": [("float", "float"), ("integer", "integer")],
    "and": [("boolean", "boolean")],
    "or": [("boolean", "boolean")],
    "equals": [("string", "string"), ("boolean", "boolean"), ("integer", "integer"), ("float", "float")],
    "not_equal": [("float", "float"), ("integer", "integer"), ("string", "string"), ("boolean", "boolean")],
    "lessthan": [("float", "float"), ("integer", "integer")],
    "lessthan_equal": [("float", "float"), ("integer", "integer")],
    "greaterthan": [("float", "float"), ("integer", "integer")],
    "greaterthan_equal": [("float", "float"), ("integer", "integer")],
    "as": [(t, "string") for t in LANGUAGE_TYPES],
    "from": [("integer", "array")]
}

# Since you can have multiple operations in an expression, we need to know each operation's precedence
# The higher the precedence, the earlier the operation is evaluated
BINARY_OPERATOR_PRECEDENCE = {
    "plus": 1,
    "minus": 1,
    "times": 2,
    "over": 2,
    "over_int": 2,
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
    "as": 0,
    "from": 0
}


# Same thing here, but for unary operators
# Which go in front of a term (after any binary operators) and apply to just it
UNARY_OPERATORS = (
    "minus",
    "not",
    "typeof",
    "stop_word",
)

UNARY_OPERATIONS = {
    "minus": lambda a: (-a.value, a.type),
    "not": lambda a: (not a.value, a.type),
    "typeof": lambda a: (a.type, "string"),
    "stop_word": lambda a: (a.value, a.type),
}

UNARY_OPERATOR_TYPE_SUPPORT = {
    "minus": ["integer", "float"],
    "not": ["boolean"],
    "typeof": LANGUAGE_TYPES,
    "stop_word": LANGUAGE_TYPES,
}

# How to convert one type('s internal `value`) to another type (also internal `value`)
LANGUAGE_TYPE_CONVERSIONS = {
    "float": {
        "integer": lambda x: int(x),
        "string": lambda x: str(x),
        "boolean": lambda x: x != 0,
        "float": lambda x: x,
    },
    "string": {
        "string": lambda x: x,
        "boolean": lambda x: x != "",
        "array": lambda x: [i for i in x],
    },
    "boolean": {
        "integer": lambda x: 1 if x else 0,
        "float": lambda x: 1.0 if x else 0.0,
        "string": lambda x: "true" if x else "false",
        "boolean": lambda x: x,
    },
    "function": {
        "string": lambda x: str(x),
    },
    "array": {
        "array": lambda x: x,
        "string": lambda x: "[" + ", ".join(as_type(i, "string")[0] for i in x) + "]",
    },
    "integer": {
        "integer": lambda x: x,
        "float": lambda x: float(x),
        "string": lambda x: str(x),
        "boolean": lambda x: x != 0,
    },
    "null": {
        "null": lambda x: None,
        "string": lambda x: "null",
        "boolean": lambda x: False,
        "integer": lambda x: 0,
        "float": lambda x: 0.0,
        "array": lambda x: [],
    }
}

# Check if the internal value is valid for a certain type of expression
LANGUAGE_TYPE_CHECKS = {
    "integer": lambda x: isinstance(x, int),
    "float": lambda x: isinstance(x, float),
    "string": lambda x: isinstance(x, str),
    "boolean": lambda x: isinstance(x, bool),
    "array": lambda x: isinstance(x, list),
    # When functions are eval-ed, they just return themselves so we can check "type". Since Function is defined in a daughter file, we can't just do a literal typecheck 
    "function": lambda x: x.type == "function",
    "null": lambda x: x is None,
}    

# First value is the language's equivalent of True, second is False
BOOLS = ("true", "false")

# Keywords are used by the parser for statements
# (Just by convention, we technically don't have to use keywords per se for statements)
KEYWORDS = (
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
    "import",
    "return",
)


# A made-up category for tokens that don't fit into any other category
MISC_SYMBOLS = ("comma", "dot", "block_start", "block_end", "array_start", "array_end")

OPEN_BRACKETS = ("open_paren",)
CLOSE_BRACKETS = ("close_paren",)

# Maps a bracket internal name to its type (brackets of a type can only be closed by a closing bracket of the same type)
BRACKET_TYPES = {
    "open_paren": "paren",
    "close_paren": "paren",
}


# Things to import before running any (top-level) program
# Import statements basically inject the code in a given file into the top-level program where they're used
CORE_IMPORTS = ("corelib.dlg",)