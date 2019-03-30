import math

def cons(x, y):
    return lambda m: m(x, y)

def car(z):
    try:
        return z(lambda p, q: p)
    except TypeError:
        raise Exception("Can't use car on non-list object", z)

def cdr(z):
    try:
        return z(lambda p, q: q)
    except TypeError:
        return None

def cadr(z):
    return car(cdr(z))

def caddr(z):
    return cadr(cdr(z))

def caadr(z):
    return car(cadr(z))

def cdadr(z):
    return cdr(cadr(z))

def cddr(z):
    return cdr(cdr(z))

def cdddr(z):
    return cdr(cddr(z))

def cadddr(z):
    return car(cdddr(z))

def append(l1, l2):
    if l1 is None:
        return l2
    return cons(car(l1), append(cdr(l1), l2))

def quotient(a, b):
    return math.floor(a / b)

def average(a, b):
    return (a + b) / 2

def length(seq):
    def ite(r, s):
        if s is None:
            return r
        return ite(r + 1, cdr(s))
    return ite(0, seq)

def lisp_map(func, seq):
    if seq is None:
        return None
    else:
        return cons(func(car(seq)), lisp_map(cdr(seq)))
    
