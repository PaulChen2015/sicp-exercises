def fixed_point(f, first_guess):
    def close_enough(v1, v2):
        return abs(v1 - v2) < 0.00001

    def trys(guess):
        nexts = f(guess)
        print("nexts = ", nexts)
        if close_enough(guess, nexts):
            return nexts
        else:
            return trys(nexts)

    return trys(first_guess)

def cube(x):
    return x ** 3

def square(x):
    return x * x

dx = 0.00001

def deriv(g):
    return lambda x: (g(x + dx) - g(x)) / dx

def newtons_method(g, guess):
    def newton_transform(g):
        return lambda x: x - (g(x) / deriv(g)(x))
    return fixed_point(newton_transform(g), guess)

def sqrt(x):
    return newtons_method(lambda y: y*y - x, 1.0)

def cubic(a, b, c):
    return lambda x: x*x*x + a*x*x + b*x + c

def double(g):
    return lambda x: g(g(x))

def inc(x):
    return x + 1

def compose(f, g):
    return lambda x: f(g(x))

def repeate(f, n):
    def iters(g, i):
        if i == 1:
            return g
        return iters(compose(f, g), i - 1)

    return iters(f, n)

        
def smooth(f):
    return lambda x: (f(x - dx) + f(x) + f(x + dx)) / 3


def n_smooth(f, n):
    return repeate(smooth(f), n)

def average(a, b):
    return (a + b) / 2

def average_damp(f):
    return lambda x: average(x, f(x))

def n_root(x, n):
    return newtons_method(lambda y: y ** n - x, 1.0)

def mul_4(x):
    return fixed_point(average_damp(lambda y: x / y ** 3), 1.0)

def floor_even(n):
    return n if (n % 2) == 0 else n - 1

def pow_of_two_ceilling(n):
    n -= 1
    n |= n >> 1
    n |= n >> 2
    n |= n >> 4
    n |= n >> 8
    n |= n >> 16
    n += 1
    return n

def pow_of_two_floor(n):
    n -= 1
    n |= n >> 1
    n |= n >> 2
    n |= n >> 4
    n |= n >> 8
    n |= n >> 16
    n += 1
    return n >> 1

def damp_times(n):
    v = pow_of_two_ceilling(n)
    tmp = v
    t = 0
    while tmp > 1:
        tmp = tmp >> 1
        t += 1
    return t if v == n else t - 1

def nth_root(x, n):
    t = damp_times(n)
    return fixed_point(repeate(average_damp(lambda y: x / (y ** (n - 1))), t), 1.0)

def iterative_improve(good_enough, improve, x):
        return lambda guess: improve(guess, x) if good_enough(improve(guess, x), x) else iterative_improve(good_enough, improve, x)(improve(guess, x))

def good_enough(guess, x):
    return abs(square(guess) - x) < dx

def improve(guess, x):
    return average(guess, x/guess)

def sqrt_iterative(x):
    return iterative_improve(good_enough, improve, x)(1.0)

def zero():
    return lambda f: lambda x: x

def add_1(n):
    return lambda f: lambda x: f(n(f)(x))

def same_parity(x, *y):
    rem = x % 2
    """return [a for a in y if a % 2 == rem]"""
    return list(filter(lambda x: x % 2 == rem, y))

def is_pow_of_two(n):
    return (n & (n - 1)) == 0

def deep_reverse(ls):
    ls.reverse()
    if ls:
        if type(ls[0]) is list:
            deep_reverse(ls[0])
        deep_reverse(ls[1:])
    
def mapping(proc, ls):
    return [proc(a) for a in ls]

def deep_reverse2(ls):
    if type(ls) is list:
        r = mapping(deep_reverse2, ls)
        r.reverse()
        return r
    else:
        return ls


def subsets(ls):
    if not ls:
        return [[]]
    rest = subsets(ls[1:])
    return rest + [ls[:1] + x for x in rest]
