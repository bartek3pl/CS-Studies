class Expression:
  def __init__(self, arg1=0, arg2=0):
    self.arg1 = arg1
    self.arg2 = arg2

  def __str__(self):
    return str(self.arg1)

  def isNumeric(self, arg):
    return str(arg).lstrip('-').isnumeric()
  
  def isProper(self):
    val1 = isinstance(self.arg1, Expression) or self.isNumeric(self.arg1) 
    val2 = isinstance(self.arg2, Expression) or self.isNumeric(self.arg2)
    return val1 and val2

  def calculate(self):
    return self.arg1.calculate()