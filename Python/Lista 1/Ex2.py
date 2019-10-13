def wallet(amount):
  denoms = [20, 10, 5, 2, 1]
  bills = []
  tempAmount = amount
  i = 0
  nextElem = i + 1

  while tempAmount != 0:
    if i == len(denoms):
      i = nextElem
      bills = []
      tempAmount = amount
    elif tempAmount - denoms[i] >= 0:
      tempAmount -= denoms[i]
      bills.append(denoms[i])
    else:
      i += 1 
      
  return bills

amount = 123
print(wallet(amount))