from expression import Expression
from program import Program

class Loop(Program):
  def __init__(self, cond, arg1):
    self.cond = cond
    self.arg1 = arg1

  def __str__(self):
    return "(loop " + str(self.cond) + " times: " + str(self.arg1) + ")"

  def execute(self):
    if isinstance(self.cond, Program):
      condCall = self.cond.execute()
    elif isinstance(self.cond, Expression):
      condCall = self.cond.calculate()

    for _ in range(condCall):
      if isinstance(self.arg1, Expression):
        val = self.arg1.calculate()
      elif isinstance(self.arg1, Program):
        val = self.arg1.execute()
      print("Current value:", val)

    return val