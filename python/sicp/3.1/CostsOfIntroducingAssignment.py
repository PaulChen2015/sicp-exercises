# 3.7, 3.8
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

    def setpasswd(newpasswd):
        nonlocal password
        password = newpasswd

    def dispatch(p, m):
        if p == password:
            if m == "withdraw":
                return withdraw
            elif m == "deposit":
                return deposit
            elif m == "getpasswd":
                return password
            elif m == "setpasswd":
                return setpasswd
            else:
                return "Unknow request " + str(m)
        else:
            raise Exception("Incorrect password")
        
    return dispatch


def make_joint(acc, passwd, newpasswd):
    oldpasswd = acc(passwd, "getpasswd")

    acc(oldpasswd, "setpasswd")(newpasswd)
    return acc


def proc():
    v = 1;
    def g(x):
        nonlocal v
        if (x == 1) and (v == 1):
            return 1
        elif x == 0:
            v = 0
        return 0
    
    return g

def testmakejoint():
    print("test make joint")
    peter_acc = make_account(100, "open-sesame")
    paul_acc = make_joint(peter_acc, "open-sesame", "rosebud")
    print(paul_acc("rosebud", "deposit")(10))
    #print(peter_acc("open-sesame", "withdraw")(20))

def testevalorder():
    print("test eval order")
    f = proc()
    print(f(0) + f(1))
    g = proc()
    print(g(1) + g(0))

if __name__ == "__main__":
    testmakejoint()
    testevalorder()
