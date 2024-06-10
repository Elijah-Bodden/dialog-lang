from config import CORE_IMPORTS
from lexer import Lexer
from nodes import Expression, ImportStatement, LiteralExpression
from dialog_parser import Parser


def interpret_program(program, env: dict[str, Expression] = {}, is_main: bool = False):
    # Making the language case-insensitive is as simple as program.lower() on the next line (TODO)
    parser = Parser(Lexer(program).tokenize(), program)
    AST = parser.parse()
    parser.print_ast()
    if is_main:
        for import_file in CORE_IMPORTS:        
            ImportStatement(LiteralExpression(import_file, "string", 0, 0, program), 0, 0, program).eval(env, interpret_program)
    for statement in AST:
        if statement.type == "import":
            # Pass the import statement this function when needed to avoid circular import hell
            statement.eval(env, interpret_program)
        else:
            statement.eval(env)
    return env


if __name__ == "__main__":
    with open("test.dlg", "r") as f:
        program = f.read()
        interpret_program(program, {}, True)
    with open("tests.dlg", "r") as f:
        program = f.read()
        interpret_program(program, {}, True)
    env = {}
    while True:
        program = input(">")
        interpret_program(program, env, True)
