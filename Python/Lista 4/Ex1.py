import timeit


def primes_imperative(n):
    def isPrime(n):
        if n <= 1:
            return False
        for i in range(2, n//2 + 1):
            if n % i == 0:
                return False
        return True

    results = []
    for i in range(2, n):
        if isPrime(i):
            results.append(i)
    return results


def primes_comprehension(n):
    return [x for x in range(2, n)
            if all(x % y != 0 for y in range(2, x//2 + 1))]


def primes_functional(n):
    return list(filter(lambda x: all(x % y != 0 for y in range(2, x//2 + 1)),
                       [x for x in range(2, n)]))


setup_code = ''' 
n = 10000
from __main__ import primes_imperative, primes_comprehension, primes_functional
'''

primes_imperative_test = '''
primes_imperative(n)
'''

primes_comprehension_test = '''
primes_comprehension(n)
'''

primes_functional_test = '''
primes_functional(n)
'''

t_imperative = timeit.timeit(
    stmt=primes_imperative_test, setup=setup_code, number=1)
t_comprehension = timeit.timeit(
    stmt=primes_comprehension_test, setup=setup_code, number=1)
t_functional = timeit.timeit(
    stmt=primes_functional_test, setup=setup_code, number=1)

print("Prime numbers imperative time    = %(time)s" %
      {"time": t_imperative})
print("Prime numbers comprehension time = %(time)s" %
      {"time": t_comprehension})
print("Prime numbers functional time    = %(time)s" %
      {"time": t_functional})

num = 20
print(primes_imperative(num))
print(primes_comprehension(num))
print(primes_functional(num))
