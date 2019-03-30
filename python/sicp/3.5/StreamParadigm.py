from Streams import *
from sicp.lisplib import *

def sqrt_improve(guess, x):
    return average(guess, x / guess)

def sqrt_stream(x):
    def guesses():
        return stream(1.0, lambda : stream_map(lambda guess: sqrt_improve(guess, x), guesses()))
    return guesses()

def pi_summands(n):
    return stream(1.0/n, lambda : stream_map((lambda x : -x), pi_summands(n + 2)))

def pi_stream():
    return scale_stream(partial_sum(pi_summands(1)), 4)

def euler_transform(s):
    s0 = s[0]
    s1 = s[1]
    s2 = s[2]
    return stream(s2 - ((s2-s1)**2 / (s0 - 2*s1 + s2)), lambda : euler_transform(s.rest()))

def make_tableau(transform, s):
    return stream(s, lambda : make_tableau(transform, transform(s)))

def accelerated_sequence(transform, s):
    return stream_map((lambda x: x.first), make_tableau(transform, s))

def exercise():
    # 3.64
    def stream_limit(s, tolerance):
        i = 0
        while True:
            x0 = s[i]
            x1 = s[i+1]
            if abs(x0 - x1) < tolerance:
                return x1
            i += 1

    sqrtoftwo = stream_limit(sqrt_stream(2), 0.0001)
    print("sqrtoftwo:", sqrtoftwo)

    # 3.65
    def ln2_stream():
        def summands(n):
            return stream(1.0/n, lambda : stream_map((lambda x: -x), summands(n+1)))
        return partial_sum(summands(1))
    print("first 8 ln2: ")
    print_n(accelerated_sequence(euler_transform, ln2_stream()), 8)


def testlesson():
    print("first 5 guesses of sqrt 2: ")
    print_n(sqrt_stream(2), 5)

    print("first 8 pi stream:")
    print_n(accelerated_sequence(euler_transform, pi_stream()), 8)

if __name__ == "__main__":
    exercise()

    
