from shared import *
from lexer import Lexer
from parser_gen import Parser

if __name__ == "__main__":
    while True:
        program = input(">")
        print(Parser(Lexer(program).tokenize(), program).parse_arithmetic())
