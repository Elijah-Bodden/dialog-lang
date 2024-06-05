from shared import *

class Parser:
    def __init__(self, tokens, program):
        self.tokens = tokens
        self.program = program
        self.pos = 0
    
    def getNextToken(self):
        return self.tokens[self.pos]

    def eat(self, allowed_types):
        token = self.getNextToken()
        print(token.type, allowed_types)
        if token.type in allowed_types:
            self.pos += 1
            return token
        else:
            raise ParserError(f"{'Expected one of: ' if type(allowed_types) == list else 'Expected '}{str(allowed_types)}, got " + self.getNextToken().type, self.getNextToken().line, self.getNextToken().col, self.program)  

    def parse_arithmetic(self):
        token = self.eat("number")
        expr = token.value

        while self.getNextToken().type != "EOS":
            token = self.eat("operator")
            if token.value == "minus":
                expr -= self.eat("number").value
            elif token.value == "plus":
                expr += self.eat("number").value
            elif token.value == "times":
                expr *= self.eat("number").value              
            elif token.value == "over":
                expr /= self.eat("number").value
        return expr
