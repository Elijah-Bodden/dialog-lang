from shared import *
# TODO: Add in-language types to expressions
# TODO: add order of operations
# TODO: add more error types for operators (currently always just "invalid operation", sometimes masks things like overflow)


class Statement:
    def __init__(self, type):
        self.type = type

    def __str__(self):
        return f"[{self.type}]"

    def __repr__(self):
        return self.__str__()
    
class PrintStatement(Statement):
    def __init__(self, expr):
        super().__init__("print")
        self.expr = expr
    
    def eval(self, env):
        evaluated_expr = self.expr.eval(env)
        print(evaluated_expr)

class IfStatement(Statement):
    def __init__(self, condition, block, else_block=None):
        super().__init__("if") 
        self.condition = condition
        self.block = block
        self.else_block = else_block
    
    def eval(self, env):
        if self.condition.eval(env):
            self.block.eval(env)
        elif self.else_block:
            self.else_block.eval(env)

class WhileStatement(Statement):
    def __init__(self, condition, block):
        super().__init__("while")
        self.condition = condition
        self.block = block
    
    def eval(self, env):
        while self.condition.eval(env):
            self.block.eval(env)

class ForStatement(Statement):
    def __init__(self, condition, block, modifier_statement):
        super().__init__("for")
        self.condition = condition
        self.modifier_statement = modifier_statement
        self.block = block

    def eval(self, env):
        while self.condition.eval(env):
            self.block.eval(env)
            self.modifier_statement.eval(env)

class ErrorStatement(Statement):
    def __init__(self, message, line, col, program):
        super().__init__("error")
        self.message = message
        self.line = line
        self.col = col
        self.program = program
    
    def eval(self, env):
        raise LanguageError(self.message.eval(env), self.line, self.col, self.program)

class FunctionCallStatement(Statement):
    def __init__(self, identifier, args):
        super().__init__("function_call")
        self.identifier = identifier
        self.args = args
    
    def eval(self, env):
        raise NotImplementedError("Function calls not implemented", 0, 0, "NOT IMPLEMENTED")

class AssignmentStatement(Statement):
    def __init__(self, identifier, expr):
        super().__init__("assignment")
        self.identifier = identifier
        self.expr = expr
    
    def eval(self, env):
        env[self.identifier] = self.expr.eval(env)

class BlockStatement(Statement):
    def __init__(self, statements):
        super().__init__("block")
        self.statements = statements
    
    def eval(self, env):
        for statement in self.statements:
            statement.eval(env)

class Expression:
    def __init__(self, type, line, col, program):
        self.type = type
        self.line = line
        self.col = col
        self.program = program
    
    def __str__(self):
        # TODO: update to print out expression?
        return f"[{self.type}]"

    def __repr__(self):
        return self.__str__()

class BinaryExpression(Expression):
    def __init__(self, left, operator, right, line, col, program):
        super().__init__("binary_expression", line, col, program)
        self.left = left
        self.operator = operator
        self.right = right
    
    def eval(self, env):
        
        try:
            return OPERATIONS[self.operator](self.left.eval(env), self.right.eval(env))
        except LanguageError as e:
            # Prevents errors from bubbling up and giving very uninformative messages
            raise e
        except:
            raise LanguageError(f"Invalid operation: {self.left} {self.operator} {self.right}", self.line, self.col, self.program)
    
class UnaryExpression(Expression):
    def __init__(self, operator, expr, line, col, program):
        super().__init__("unary_expression", line, col, program)
        self.operator = operator
        self.expr = expr
    
    def eval(self, env):
        try:
            print(self.operator)
            return OPERATIONS[self.operator](self.expr.eval(env))
        except LanguageError as e:
            # Prevents errors from bubbling up and giving very uninformative messages
            raise e
        except:
            raise LanguageError(f"Invalid operation: {self.operator} {self.expr}", self.line, self.col, self.program)
    
class LiteralExpression(Expression):
    # TODO: type
    def __init__(self, literal, type, line, col, program):
        super().__init__(f"literal {type}", line, col, program)
        self.literal = literal
    
    def eval(self, env):
        return self.literal
    
    def __str__(self):
        return super().__str__() + f" {self.literal}"
    
class IdentifierRefrence(Expression):
    def __init__(self, identifier, line, col, program):
        super().__init__("identifier", line, col, program)
        self.identifier = identifier
    
    def eval(self, env):
        try:
            return env[self.identifier]
        except KeyError:
            raise LanguageError(f"Identifier not found: {self.identifier}", self.line, self.col, self.program)