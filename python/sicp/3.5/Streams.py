from sicp.lisplib import *
def force(delayed_obj):
    return delayed_obj()

def memo_proc(proc):
    executed = False
    result = False
    def memo():
        nonlocal executed
        nonlocal result
        if not executed:
            result = proc()
            executed = True
        return result
    return memo

def delay(exp):
    return memo_proc(lambda : exp)


def range_from(n):
    return stream(n, lambda : range_from(n + 1))

class stream:
    def __init__(self, first, rest):
        self.first = first
        self.rest = memo_proc(rest)

    def __repr__(self):
        return "stream[{0}, <rest elements>]".format(self.first)

    def __iter__(self):
        while True:
            yield self.first
            s = self.rest()
            self.first = s.first
            self.rest = s.rest

    def __getitem__(self, index):
        if index < 0:
            raise IndexError(index)
        s = self
        while index > 0:
            s = s.rest()
            index -= 1
        return s.first

    def of(self, seq):
        if seq:
            return stream(seq[0], lambda : self.of(seq[1:]))


def stream_filter(pred, s):
    if pred(s.first):
        return stream(s.first, lambda : stream_filter(pred, s.rest()))
    return stream_filter(pred, s.rest())

def stream_map(proc, *s):
    #if s is None:
        #return s
    #return stream(proc(s.first), lambda : stream_map(proc, s.rest()))"""

    firsts = []
    rests = []
    for x in s:
        firsts.append(x.first)
        rests.append(x.rest)
    return stream(proc(*firsts), lambda : stream_map(proc, *apply(*rests)))

def apply(*seqs):
    r = []
    for s in seqs:
        r.append(s())
    return r


def fibgen(a, b):
    return stream(a, lambda : fibgen(b, a + b))


def divisible(x, y):
    return (x % y) == 0


def sieve(s):
    return stream(s.first, lambda : sieve(stream_filter(lambda x: not divisible(x, s.first), s.rest())))


def testinfinitestream():
    ints = range_from(1)
    print("7th integer", ints[6])

    fibs = fibgen(0, 1)
    print("11th fib", fibs[10])

    primes = sieve(range_from(2))
    print("21th prime", primes[20])


def scale_stream(s, factor):
    return stream_map(lambda x : x * factor, s)

def double():
    return stream(1, lambda : scale_stream(double(), 2))

def add_streams(s1, s2):
    return stream_map((lambda a, b : a + b), s1, s2)

def ones():
    return stream(1, lambda : ones())

def ints():
    return stream(1, lambda : add_streams(ones(), ints()))


def primes():
    return stream(2, lambda : stream_filter(isprime, range_from(3)))

def isprime(n):
    def ite(ps):
        if ps.first ** 2 > n:
            return True
        elif divisible(n, ps.first):
            return False
        else:
            return ite(ps.rest())
    return ite(primes())

def testiimplicitstream():
    d = double()
    print("15th double stream", d[14])
    print("3rd int", ints()[2])
    print("3rd recurisive prime", primes()[2])

def print_n(s, n):
    for i in range(n):
        print(s[i], end=' ')
    print()


def partial_sum(s):
    def ite():
        return stream(0, lambda : add_streams(ite(), s))
    return ite().rest()
    
def exercise():
    # 3.54
    def mul_streams(s1, s2):
        return stream_map(lambda a, b: a * b, s1, s2)

    def factorials():
        return stream(1, lambda : mul_streams(factorials(), range_from(2)))

    print("fifth factorial is", factorials()[4])

    #3.55
    partials = partial_sum(range_from(1))
    print("first 5 partial sum of integer", end=' ')
    print_n(partials, 5)

    #3.56
    def merge(s1, s2):
        if s1 is None:
            return s2
        elif s2 is None:
            return s1
        else:
            s1car = s1.first
            s2car = s2.first
            if s1car < s2car:
                return stream(s1car, lambda : merge(s1.rest(), s2))
            elif s1car > s2car:
                return stream(s2car, lambda : merge(s1, s2.rest()))
            else:
                return stream(s2car, lambda : merge(s1.rest(), s2.rest()))

    def S():
        return stream(1, lambda : merge(scale_stream(S(), 2), merge(scale_stream(S(), 3), scale_stream(S(), 5))))

    print("first 10 factors of 2 3 5 are ", end=' ')
    print_n(S(), 10)


    # 3.58
    def expand(num, den, radix):
        return stream(quotient(num * radix, den), lambda : expand((num*radix) % den, den, radix))

    e1 =  expand(1, 7, 10)
    print_n(e1, 12)

    e2 = expand(3, 8, 10)
    print_n(e2, 12)


    # 3.59
    def integrate_series(s):
        #def ite(ints, ss):
            #return stream(ss.first/ints.first, lambda : ite(ints.rest(), ss.rest()))
        #return ite(range_from(1), s)
        return mul_streams(s, stream_map((lambda x: 1/x), range_from(1)))

    def exp_series():
        return stream(1, lambda : integrate_series(exp_series()))

    print_n(exp_series(), 10)

    def cosine_series():
        return stream(1, lambda : scale_stream(integrate_series(sine_series()), -1))

    def sine_series():
        return stream(0, lambda : integrate_series(cosine_series()))

    cosines = cosine_series()
    print_n(cosines, 10)
    sines = sine_series()
    print_n(sines, 10)

    #3.60 something wrong, isn't it?
    def mul_series(s1, s2):
        return mul_streams(partial_sum(s1), partial_sum(s2))

    sincos = add_streams(mul_series(cosines, cosines), mul_series(sines, sines))
    print("test mul_series by sinx**2 + cosx**2 = 1 :")
    print_n(sincos, 10)
        
if __name__ == "__main__":
    #testinfinitestream()
    #testiimplicitstream()
    exercise()

