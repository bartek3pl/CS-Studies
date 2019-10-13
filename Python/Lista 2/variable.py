from exceptions import ValueIsNotAssigned, VariableDoesNotExist
from expression import Expression

class Variable(Expression):
  def __init__(self, arg):
    self.arg = arg
    self.pair = {}

  def __str__(self):
    try:
      if not self.arg in self.pair:
        raise ValueIsNotAssigned
      return str(self.arg)
    except ValueIsNotAssigned:
      print("Variable has no value assigned.")
      return ""

  def setValue(self, val):
    self.pair[self.arg] = val.calculate()
    return self.pair[self.arg]

  def getValue(self):
    try:
      if not self.arg in self.pair:
        raise ValueIsNotAssigned
      return self.pair[self.arg]
    except ValueIsNotAssigned:
      print("Variable has no value assigned.")
      return ""

  def delVariable(self):
    try:
      if not self.arg in self.pair:
        raise VariableDoesNotExist
      self.pair.pop(self.arg)
    except VariableDoesNotExist:
      print("Variable does not exist.")
      return ""

  def calculate(self):
    return self.getValue()
