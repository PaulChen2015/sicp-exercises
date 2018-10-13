import copy
def cons(x, y):
    if x is None:
        if type(y) is list:
            return y
        return [y]
    elif y is None:
        return [x]

    r = [x]
    if type(y) is list:
        r.extend(y)
    else:
        r.append(y)
    return r

def car(pair):
    if type(pair) is not list:
        raise Exception("the value of ", pair, "is not type of list")

    if len(pair) == 0:
        return None
    return copy.deepcopy(pair[0])


def cdr(pair):
    if pair is None:
        return None
    rest = pair[1:]
    if len(rest) == 0:
        return None
    return copy.deepcopy(rest)

def cadr(pair):
    return car(cdr(pair))

def append(l1, l2):
    if l1 is None:
        return l2
    return cons(car(l1), append(cdr(l1), l2))


def set_cdr(x, y):
    x[1:] = y
