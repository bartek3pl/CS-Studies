import timeit
import itertools
from prettytable import PrettyTable


def isPrime(n):
    if n <= 1:
        return False
    for i in range(2, n//2 + 1):
        if n % i == 0:
            return False
    return True


def primes_imperative(n):
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


def primes_iterator():
    i = itertools.count(2)
    while(True):
        p = next(i)
        if(isPrime(p)):
            yield p


setup_code = '''
n1 = 10
n2 = 100
n3 = 1000
n4 = 2000
from __main__ import primes_imperative, primes_comprehension, primes_functional, primes_iterator
'''

primes_imperative_test_1 = '''
primes_imperative(n1)
'''

primes_comprehension_test_1 = '''
primes_comprehension(n1)
'''

primes_functional_test_1 = '''
primes_functional(n1)
'''

primes_iterator_test_1 = '''
f = primes_iterator()
for _ in range(n1):
    next(f)
'''

primes_imperative_test_2 = '''
primes_imperative(n2)
'''

primes_comprehension_test_2 = '''
primes_comprehension(n2)
'''

primes_functional_test_2 = '''
primes_functional(n2)
'''

primes_iterator_test_2 = '''
f = primes_iterator()
for _ in range(n2):
    next(f)
'''

primes_imperative_test_3 = '''
primes_imperative(n3)
'''

primes_comprehension_test_3 = '''
primes_comprehension(n3)
'''

primes_functional_test_3 = '''
primes_functional(n3)
'''

primes_iterator_test_3 = '''
f = primes_iterator()
for _ in range(n3):
    next(f)
'''

primes_imperative_test_4 = '''
primes_imperative(n4)
'''

primes_comprehension_test_4 = '''
primes_comprehension(n4)
'''

primes_functional_test_4 = '''
primes_functional(n4)
'''

primes_iterator_test_4 = '''
f = primes_iterator()
for _ in range(n4):
    next(f)
'''

t_imperative_1 = timeit.timeit(
    stmt=primes_imperative_test_1, setup=setup_code, number=1)
t_comprehension_1 = timeit.timeit(
    stmt=primes_comprehension_test_1, setup=setup_code, number=1)
t_functional_1 = timeit.timeit(
    stmt=primes_functional_test_1, setup=setup_code, number=1)
t_iterator_1 = timeit.timeit(
    stmt=primes_iterator_test_1, setup=setup_code, number=1)

t_imperative_2 = timeit.timeit(
    stmt=primes_imperative_test_2, setup=setup_code, number=1)
t_comprehension_2 = timeit.timeit(
    stmt=primes_comprehension_test_2, setup=setup_code, number=1)
t_functional_2 = timeit.timeit(
    stmt=primes_functional_test_2, setup=setup_code, number=1)
t_iterator_2 = timeit.timeit(
    stmt=primes_iterator_test_2, setup=setup_code, number=1)

t_imperative_3 = timeit.timeit(
    stmt=primes_imperative_test_3, setup=setup_code, number=1)
t_comprehension_3 = timeit.timeit(
    stmt=primes_comprehension_test_3, setup=setup_code, number=1)
t_functional_3 = timeit.timeit(
    stmt=primes_functional_test_3, setup=setup_code, number=1)
t_iterator_3 = timeit.timeit(
    stmt=primes_iterator_test_3, setup=setup_code, number=1)

t_imperative_4 = timeit.timeit(
    stmt=primes_imperative_test_4, setup=setup_code, number=1)
t_comprehension_4 = timeit.timeit(
    stmt=primes_comprehension_test_4, setup=setup_code, number=1)
t_functional_4 = timeit.timeit(
    stmt=primes_functional_test_4, setup=setup_code, number=1)
t_iterator_4 = timeit.timeit(
    stmt=primes_iterator_test_4, setup=setup_code, number=1)

results_table = PrettyTable()

results_table.field_names = ["N", "Imperative",
                             "Comprehension", "Functional", "Iterator"]

results_table.add_row(
    ["10", t_imperative_1, t_comprehension_1, t_functional_1, t_iterator_1])
results_table.add_row(
    ["100", t_imperative_2, t_comprehension_2, t_functional_2, t_iterator_2])
results_table.add_row(
    ["1000", t_imperative_3, t_comprehension_3, t_functional_3, t_iterator_3])
results_table.add_row(
    ["2000", t_imperative_4, t_comprehension_4, t_functional_4, t_iterator_4])

print(results_table)
