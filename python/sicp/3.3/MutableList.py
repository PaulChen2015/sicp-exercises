from sicp.lisplib import *
# 3.12, 3.14, 3.17, 3.18
def mystery(x):
    def loop(x, y):
        if len(x) == 0:
            return y
        else:
            temp = x[1:]
            x[1:] = y
            return loop(temp, x)

    return loop(x, [])


def count_pairs(x):
    def itr(r, ls):
        if type(ls) is not list:
            return r
        first = car(ls)
        if type(first) is list:
            return itr(r + 1, first) + itr(0, cdr(ls))
        else:
            return itr(r, cdr(ls))
                       
    if type(x) is list:
        return itr(1, x)
    return 0


def append_mut(x, y):
    if y is not None:
        if type(y) is list:
            x.extend(y)
        else:
            x.append(y)
    return x

def contains_cycle(ls):
    for x in ls:
        if x == ls:
            return True
    return False

def testappend():
    print("test append")
    x = ['a', 'b']
    y = ['c', 'd']
    z = append(x, y)
    print(z)
    print(cdr(x))

    w = append_mut(x, y)
    print(w)
    print(cdr(x))

if __name__ == "__main__":
    testappend()
    print("test mystery")
    print(mystery(['a', 'b', 'c', 'd']))
    print("test count pairs")
    print(count_pairs([1, [2, [3]], [[4, 5], 6, [[7]]]]))
