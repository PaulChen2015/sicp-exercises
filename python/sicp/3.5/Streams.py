
def force(delayed_obj):
    return delayed_obj()

def memo_proc(proc):
    executed = False
    result = False
    def memo():
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
        self.rest = rest

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


def stream_filter(pred, s):
    if pred(s.first):
        return stream(s.first, lambda : stream_filter(pred, s.rest()))
    return stream_filter(pred, s.rest())
                                                           

def fibgen(a, b):
    return stream(a, lambda : fibgen(b, a + b))


def divisible(x, y):
    return (x % y) == 0


def sieve(s):
    return stream(s.first, lambda : sieve(stream_filter(lambda x: not divisible(x, s.first), s.rest())))

def primes():
    return sieve(range_from(2))
