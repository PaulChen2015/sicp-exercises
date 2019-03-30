from Streams import *

def integral(delayed_integrand, initial_value, dt):
    def ints():
        return stream(initial_value, lambda : add_streams(scale_stream(delayed_integrand(), dt), ints()))
    return ints()

def solve(f, y0, dt):
    def y():
        return integral(lambda : dy(), y0, dt)

    def dy():
        return stream_map(f, y())
    return y()


if __name__ == "__main__":
    #e = solve(lambda y: y, 1, 0.001)
    #print(e[1000]) #cause MemoryError
