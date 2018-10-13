import random

def monte_carlo(trials, experiment):
    def itr(remaining, passed):
        if remaining == 0:
            return passed / trials
        elif experiment():
            return itr(remaining - 1, passed + 1)
        else:
            return itr(remaining - 1, passed)

    return itr(trials, 0)


def estimate_integral(x1, x2, y1, y2, trials):
    a = x2 - x1
    b = y2 - y1
    r = min(a, b)
    x = a / 2
    y = b / 2
    rectangle_area = r ** 2
    def cesaro_test():
        xp = random.uniform(x1, x2)
        yp = random.uniform(y1, y2)
        tmp = (xp - x) ** 2 + (yp - y) ** 2
        #print("x=",x,"y=",y,"r=",r,"xp=",xp,"yp=",yp,"tmp=",tmp,"rectangle_area=",rectangle_area)
        return tmp <= rectangle_area

    fraction = monte_carlo(trials, cesaro_test)

    return a * b * fraction


if __name__ == "__main__":
    circle_area = estimate_integral(2, 8, 4, 10, 500)
    pi = circle_area / (3 ** 2)
    print("pi=", pi)
