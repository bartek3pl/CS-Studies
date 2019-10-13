from expression import Expression
from program import Program

class IfCond(Program):
  def __init__(self, cond, arg1, arg2):
    self.cond = cond
    self.arg1 = arg1
    self.arg2 = arg2

  def __str__(self):
    return "(if " + str(self.cond) + ": " + str(self.arg1) + " else: " + str(self.arg2) + ")"

  def execute(self):
    if isinstance(self.cond, Program):
      condCall = self.cond.execute()
    elif isinstance(self.cond, Expression):
      condCall = self.cond.calculate()

    if condCall != 0:
      if isinstance(self.arg1, Program):
        return self.arg1.execute()
      elif isinstance(self.arg1, Expression):
        return self.arg1.calculate()
    else:
      if isinstance(self.arg2, Program):
        return self.arg2.execute()
      elif isinstance(self.arg2, Expression):
        return self.arg2.calculate()

    

