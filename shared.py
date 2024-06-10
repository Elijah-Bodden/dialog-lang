def indent(string: str):
    lines = string.split("\n")
    lines = [f"{' ' * 2}{line}" for line in lines]
    return "\n".join(lines)

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


# Errors
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
