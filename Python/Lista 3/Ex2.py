import math

def square_root(n):
  res = 0
  for i in range(1, n+1):
    res += 2*i - 1
    if res >= n:
      return res / i
  return res

def print_results(n):
  results = {}
  for i in range(1, n):
    results[i] = square_root(i)
  print(results)
  return ""

print_results(17)

