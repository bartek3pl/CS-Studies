import timeit
import math
from functools import reduce


def perfect_imperative(n):
    def sumOfFactors(n):
        sum = 0
        for i in range(1, math.ceil(n/2) + 1):
            if n % i == 0:
                sum += i
        return sum

    results = []
    for i in range(1, n):
        if i == sumOfFactors(i):
            results.append(i)
    return results


def perfect_comprehension(n):
    return [x for x in range(1, n)
            if sum(y for y in range(1, math.ceil(x/2) + 1) if x % y == 0) == x]


def perfect_functional(n):
    return list(filter(lambda x: sum(y for y in range(1, math.ceil(x/2) + 1) if x % y == 0) == x,
                       [x for x in range(1, n)]))


def perfect_more_functional(n):
    return list(filter(lambda x: reduce((lambda a, b: a + b),
                                        filter(lambda y: x % y == 0,
                                               [y for y in range(1, math.ceil(x/2) + 1)])) == x,
                       [x for x in range(1, n)]))


setup_code = '''
n = 2000
from __main__ import perfect_imperative, perfect_comprehension, perfect_functional, perfect_more_functional
'''

perfect_imperative_test = '''
perfect_imperative(n)
'''

perfect_comprehension_test = '''
perfect_comprehension(n)
'''

perfect_functional_test = '''
perfect_functional(n)
'''

perfect_more_functional_test = '''
perfect_more_functional(n)
'''

t_imperative = timeit.timeit(
    stmt=perfect_imperative_test, setup=setup_code, number=1)
t_comprehension = timeit.timeit(
    stmt=perfect_comprehension_test, setup=setup_code, number=1)
t_functional = timeit.timeit(
    stmt=perfect_functional_test, setup=setup_code, number=1)
t_more_functional = timeit.timeit(
    stmt=perfect_more_functional_test, setup=setup_code, number=1)

print("Perfect numbers imperative time      = %(time)s" %
      {"time": t_imperative})
print("Perfect numbers comprehension time   = %(time)s" %
      {"time": t_comprehension})
print("Perfect numbers functional time      = %(time)s" %
      {"time": t_functional})
print("Perfect numbers more functional time = %(time)s" %
      {"time": t_more_functional})

num = 2000
print(perfect_imperative(num))
print(perfect_comprehension(num))
print(perfect_functional(num))
print(perfect_more_functional(num))
