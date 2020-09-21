import math
from fractions import Fraction
from random import uniform, randint
import decimal as dec

def log10_floor(f):
    b, k = 1, -1
    while b <= f:
        b *= 10
        k += 1
    return k

def log10_ceil(f):
    b, k = 1, 0
    while b < f:
        b *= 10
        k += 1
    return k

def str_of_pos_float_hi0(prec, x):
    assert x > 0
    q = Fraction(x)
    n = int(q)
    if n > 0:
        k = log10_floor(n) + 1
        if k >= prec:
            b = 10 ** (k - prec)
            r, e = n // b, k - prec
        else:
            b = 10 ** (prec - k)
            r, e = n * b + int((q - n) * b), k - prec
    else:
        k = log10_floor(int(1 / q))
        b = 10 ** (k + prec)
        r, e = int(q * b), -(k + prec)
    if r * Fraction(10) ** e < q:
       r += 1
    s = str(r)
    if len(s) > prec:
        s = s[:-1]
        e += 1
    e += prec - 1
    s = f'{s[0]}.{s[1:]}'
    if e == 0:
        return s
    return s + ('e+' if e > 0 else 'e') + str(e)

def str_of_pos_float_hi1(prec, x):
    assert x > 0
    m, exp = math.frexp(x)
    m, exp = int(math.ldexp(m, 53)), exp - 53
    mask = (1 << abs(exp)) - 1
    if exp >= 0:
        n, rem = m << exp, 0
    else:
        n, rem = m >> -exp, m & mask
    if n > 0:
        k = log10_floor(n) + 1
        if k >= prec:
            b = 10 ** (k - prec)
            (r, rem2), e = divmod(n, b), k - prec
            rem2 = rem2 or rem
        else:
            b = 10 ** (prec - k)
            t = rem * b
            t, rem2 = t >> -exp, t & mask
            r, e = n * b + t, k - prec
    else:
        k = log10_floor((1 << -exp) // rem)
        b = 10 ** (k + prec)
        t = rem * b
        r, rem2, e = t >> -exp, t & mask, -(k + prec)
    if rem2:
        r += 1
    s = str(r)
    if len(s) > prec:
        s = s[:-1]
        e += 1
    e += prec - 1
    s = f'{s[0]}.{s[1:]}'
    if e == 0:
        return s
    return s + ('e+' if e > 0 else 'e') + str(e)

# print(str_of_pos_float_hi(2, 230454523525e+100))

def decimal_test(prec, x, s=None):
    if s is None:
        s = str_of_pos_float_hi1(prec, x)
    with dec.localcontext() as ctx:
        ctx.prec = prec
        ctx.rounding = dec.ROUND_UP
        v = +dec.Decimal(x)
        t = +dec.Decimal(s)
        if v != t:
            print(f'Error: decimal = {v}, my = {s} (prec = {prec}, x = {x})')

def tests(n, a, b):
    for _ in range(n):
        x = uniform(a, b)
        prec = randint(1, 15)
        decimal_test(prec, x)

def tests2(n):
    for _ in range(n):
        prec = randint(1, 15)
        t = randint(-100, 100)
        decimal_test(prec, 2.0 ** t)

tests(10000, 1e-300, 1)
tests(10000, 0.5, 1000)
tests(10000, 1e+10, 1e+100)
tests(10000, 1e-300, 1e+300)
tests2(10000)

#print(str_of_pos_float_hi1(1, 0.47))
#print(str_of_pos_float_hi1(1, 0.5))
# print(str_of_pos_float_hi1(100, 0.3))

def check_ocaml_results(fname):
    with open(fname, 'r') as f:
        for line in f:
            x, prec, s0, s1 = line.strip().split(',')
            decimal_test(int(prec), float(x), s0)
            decimal_test(int(prec), float(x), s1)

check_ocaml_results('out.txt')