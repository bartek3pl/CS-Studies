from const import Const
from arithmetic_operations import Add, Sub, Mul, Div
from variable import Variable
from if_cond import IfCond
from loop import Loop
from program import Program

expr = Sub(Add(Const(-2), Const(10)),
          (Div(Mul(Const(20), Const(3)),
              Const(5))))

print(str(expr) + " = " + str(expr.calculate()))

varX = Variable("x")
varX.setValue(Mul(Const(2), Const(5)))

varY = Variable("y")
varY.setValue(Const(3))

expr2 = Sub(Add(Const(1), varY), varX) #-6

print(str(expr2) + " = " + str(expr2.calculate()))

expr3 = IfCond(IfCond(expr2, Const(0), Const(1)), 
              Sub(expr2, Const(10)), #-16
              Add(expr2, Const(10))) #4

print(str(expr3) + " = " + str(expr3.execute()))

#expr4 = varX.setValue(Sub(varX, Const(1)))
#print(str(expr4) + " = " + str(varX.getValue()))
#expr4 = varX.setValue(Sub(varX, Const(1)))
#print(str(expr4) + " = " + str(varX.getValue()))

#print(varX) #x
#print(varX.setValue(Const(1))) #x = 1
#print(varX.getValue()) #1

expr5 = Loop(IfCond(Const(0), Const(1), Const(2)), Const(7)) 

print(str(expr5) + " = " + str(expr5.execute()))



