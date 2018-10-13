from math import sqrt
def withdraw(balance):
    def draw(amount):
        nonlocal balance
        if balance >= amount:
            balance -= amount
            return balance
        else:
            return "Insufficient funds"
    return draw

def make_accumulator(v):
    def accumulate(x):
        nonlocal v
        v += x
        return v
    return accumulate

def make_monitored(f):
    calltimes = 0
    def countcalls():
        return calltimes

    def resetcount():
        nonlocal calltimes
        calltimes = 0

    def callfunc(x):
        nonlocal calltimes
        calltimes += 1
        return f(x)

    def dispatch(m):
        if m == "how-many-calls?":
            return countcalls()
        elif m == "reset-count":
            return resetcount()
        else:
            return callfunc(m)

    return dispatch


def make_account(balance, password):
    def withdraw(amount):
        nonlocal balance
        if balance >= amount:
            balance -= amount
            return balance
        else:
            return "Insufficient funds"

    def deposit(amount):
        nonlocal balance
        balance += amount
        return balance

    def callthecops(a):
        return "call the cops"

    wrongpasswdcount = 0
    def dispatch(p, m):
        nonlocal wrongpasswdcount
        if p == password:
            if m == "withdraw":
                return withdraw
            elif m == "deposit":
                return deposit
            else:
                return "Unknow request " + str(m)
        else:
            wrongpasswdcount += 1
            if wrongpasswdcount > 7:
                return callthecops
            raise Exception("Incorrect password")
        
    return dispatch

if __name__ == '__main__':
    print("test make_accumulator")
    print("init accumulator with value 5")
    accumu = make_accumulator(5)
    print("call accumulator with value 10, result is", accumu(10))
    print("call accumulator with value 10, result is", accumu(10))
    print("------------------")
    
    print("test make_monitored")
    print("pass function sqrt to make_monitored stored as monitor")
    monitor = make_monitored(sqrt)
    print("call monitor(100), result is", monitor(100))
    print("call monitor(100), result is", monitor(25))
    print("call monitor('how-many-calls?'), result is", monitor("how-many-calls?"))
    monitor('reset-count')
    print("first call monitor('reset-count'), then call monitor('how-many-calls?'), result is", monitor("how-many-calls?"))

    print("test make_account")
    acc = make_account(100, "secret-passwrod")
    print(acc("secret-passwrod", "withdraw")(40))
    for x in range(8):
        try:
            print(acc("some-passwrod", "deposit")(50))
        except:
            print("Incorrect password")
