def vat_invoice(list):
  sum = 0
  for elem in list:
    sum += elem
  return sum*0.23

def vat_receipt(list):
  sum = 0
  for elem in list:
    sum += elem*0.23
  return sum

shopping = [0.2, 0.5, 4.59, 6]
print("Invoice vat =", vat_invoice(shopping))
print("Receipt vat =", vat_receipt(shopping))
print("Equal?", vat_invoice(shopping) == vat_receipt(shopping))
