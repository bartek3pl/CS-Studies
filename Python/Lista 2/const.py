from expression import Expression
from exceptions import ValueIsNaN

class Const(Expression):
  def __init__(self, arg1):
    self.arg1 = arg1
    self.arg2 = 0

  def __str__(self):
    return str(self.arg1)
 
  def calculate(self):
    try:
      if not self.isProper():
        raise ValueIsNaN
      else:
        return self.arg1
    except ValueIsNaN:
      print("Value is not a number: %s" %self)
      return ""

