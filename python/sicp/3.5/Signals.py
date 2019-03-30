from Streams import *

def integral(integrand, initial_value, dt):
    def integer():
        return stream(initial_value, add_streams(scale_stream(integrand, dt), integer()))
    return integer()


def exercise():
    # 3.73
    def RC(R, C, time):
        def circuit(currents, v0):
            return add_streams(scale_stream(currents, R), stream(v0, lambda : scale_stream(currents, (1/C)*time)))
        return circuit

    RC1 = RC(5, 1, 0.5)
    n = 6
    print("first %d RC1: " % n)
    print_n(RC1(range_from(1), 0.1), n)

    # 3.74
    def zero_crossings(sense_data):
        return stream_map(sign_change_detector, sense_data, stream(0, lambda : sense_data))

    def same_sign(a, b):
        return ((a >= 0) and (b >=0)) or ((a < 0) and (b < 0))
    
    def sign_change_detector(new, old):
        if same_sign(new, old):
            return 0
        elif (new >= 0) and (old < 0):
            return 1
        else:
            return -1

    data = [1,2,1.5,1,0.5,-0.1,-2,-3,-2,-0.5,0.2,3,4]
    data_stream = stream(None, None).of(data)
    signals = zero_crossings(data_stream)
    print("sense data: ", data)
    print("signals   : ", end=' ')
    print_n(signals, 13)

if __name__ == "__main__":
    exercise()

