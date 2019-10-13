from expression import Expression

class Program(Expression):
  def __init__(self, arg1=0, arg2=0, cond=0):
    self.arg1 = arg1
    self.arg2 = arg2
    self.cond = cond

  def __str__(self):
    return str(self.arg1)

  def isProper(self):
    val1 = isinstance(self.arg1, Program)
    val2 = isinstance(self.arg2, Program)
    if val1 and val2:
      return True
    return False

  def execute(self):
    return self.arg1.execute()
