from Streams import *
import random
import math

def random_numbers():
    return stream(random.randrange(10000), lambda : random_numbers())

def map_successive_pair(f, s):
    return stream(f(s.first, s.rest().first), lambda : map_successive_pair(f, s.rest().rest()))

def cesaro_stream():
    return map_successive_pair((lambda r1,r2: math.gcd(r1,r2)==1), random_numbers())

def monte_carlo(experiment_stream, passed, failed):
    def nexts(passed, failed):
        return stream(passed / (passed+failed), lambda : monte_carlo(experiment_stream.rest(), passed, failed))

    if experiment_stream.first:
        return nexts(passed + 1, failed)
    else:
        return nexts(passed, failed + 1)

def pi(): #may raise ZeroDivisionError
    return stream_map((lambda p: math.sqrt(6/p)), monte_carlo(cesaro_stream(), 0, 0))


if __name__ == "__main__":
    print(pi()[1000])
