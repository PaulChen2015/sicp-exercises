def average(a, b):
    return (a+b)/2

def sqrt_improve(guess, x):
    return average(guess, x/guess)

def sqrt_generator(x, limit=True):
    guess = 1.0
    while limit:
        if isinstance(limit, int):
            limit -= 1
        guess = sqrt_improve(guess, x)
        yield guess


def pi_gen(n):
    def pi_summands(n, result, tmp):
        while True:
            if tmp > 0:
                tmp = -1.0/n
            else:
                tmp = 1.0/n
            result += tmp * 4
            yield result
            n += 2
    return pi_summands(n, 0, -1)


def euler_transform(s):
    def ite(seq):
        a = next(seq)
        b = next(seq)
        c = next(seq)
        while True:
            yield c - ((c-b)**2/(a - 2*b + c))
            a, b = b, c
            c = next(seq)
    return ite(iter(s))

def make_tableau(tansform, s):
    t = tansform(s)
    while True:
        yield t
        t = tansform(t)

def accelerated_seq(transform, s):
    t = iter(make_tableau(transform, s))
    while True:
        yield next(iter(next(t)))
