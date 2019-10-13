from expression import Expression
from exceptions import ValueIsNaN, DivideByZero
from const import Const
from variable import Variable

class Add(Expression):
  def __init__(self, arg1, arg2):
    self.arg1 = arg1
    self.arg2 = arg2

  def __str__(self):
    return "(" + str(self.arg1) + " + " + str(self.arg2) + ")"

  def calculate(self):
    try:
      if not self.isProper():
        raise ValueIsNaN
      else:
        return self.arg1.calculate() + self.arg2.calculate()
    except ValueIsNaN:
      print("Value is not a number: %s" %self)
      return ""
    
class Sub(Expression):
  def __init__(self, arg1, arg2):
    self.arg1 = arg1
    self.arg2 = arg2

  def __str__(self):
    return "(" + str(self.arg1) + " - " + str(self.arg2) + ")"
   
  def calculate(self):
    try:
      if not self.isProper():
        raise ValueIsNaN
      else:
        return self.arg1.calculate() - self.arg2.calculate()
    except ValueIsNaN:
      print("Value is not a number: %s" %self)
      return ""
    
class Mul(Expression):
  def __init__(self, arg1, arg2):
    self.arg1 = arg1
    self.arg2 = arg2

  def __str__(self):
    return "(" + str(self.arg1) + " * " + str(self.arg2) + ")"
   
  def calculate(self):
    try:
      if not self.isProper():
        raise ValueIsNaN
      else:
        return self.arg1.calculate() * self.arg2.calculate()
    except ValueIsNaN:
      print("Value is not a number: %s" %self)
      return ""

class Div(Expression):
  def __init__(self, arg1, arg2):
    self.arg1 = arg1
    self.arg2 = arg2

  def __str__(self):
    return "(" + str(self.arg1) + " / " + str(self.arg2) + ")"
   
  def calculate(self):
    try:
      if not self.isProper():
        raise ValueIsNaN
      elif self.arg2.calculate() == 0:
        raise DivideByZero
      else:
        return self.arg1.calculate() / self.arg2.calculate()
    except ValueIsNaN:
      print("Value is not a number: %s" %self)
      return ""
    except DivideByZero:
      print("Value can not be divided by zero: %s" %self)
      return ""