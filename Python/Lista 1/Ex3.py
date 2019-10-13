import math

def rhombus(n):
  length = 2*n-1
  start = math.ceil(length / 2)
  end = start

  for i in range (0, length+2):
    for j in range (0, length+2):
      if (j >= start and j <= end):
        print('#', end='')
      else:
        print(' ', end='')
    if i <= length / 2:
      start -= 1
      end += 1
    else:
      start += 1
      end -= 1

    print('')
  return ''

n = 4
print(rhombus(n))