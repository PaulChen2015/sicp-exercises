from math import sqrt

class connector:
    def __init__(self):
        self.value = False
        self.informant = False
        self.constraints = []

    def __repr__(self):
        return "connector[value={0} informant={1} constraints={2}]".format(self.value, self.informant, self.constraints)

    def hasvalue(self):
        return self.informant
 
    def setvalue(self, newval, setter):
        if not self.hasvalue():
            self.value = newval
            self.informant = setter
            self.foreachexcept(setter, lambda x: x.processnewvalue, self.constraints)
        elif self.value != newval:
            raise Exception("Constradiction", [self.value, newval])
        else:
            print("setvalue call ignored newval=", newval, "setter=", setter)

    def forgetvalue(self, retractor):
        if self.informant == retractor:
            self.informant = False
            self.foreachexcept(retractor, lambda x: x.processforgetvalue, self.constraints)
        else:
            print("forgetvalue call ignored. retractor=", retractor)

    def connect(self, newconstraint):
        if not self.constraints.__contains__(newconstraint):
            self.constraints.insert(0, newconstraint)
        if self.hasvalue():
            newconstraint.processnewvalue()

        print("connect complete. newconstraint=", newconstraint)

    def foreachexcept(self, exception, procedure, ls):
        for item in ls:
            if item != exception:
                procedure(item)()


class adder:
    def __init__(self, a1, a2, sum):
        self.a1 = a1
        self.a2 = a2
        self.sum = sum
        self.a1.connect(self)
        self.a2.connect(self)
        self.sum.connect(self)

    def processnewvalue(self):
        if self.a1.hasvalue() and self.a2.hasvalue():
            self.sum.setvalue(self.a1.value + self.a2.value, self)
        elif self.a1.hasvalue() and self.sum.hasvalue():
            self.a2.setvalue(self.sum.value - self.a1.value, self)
        elif self.a2.hasvalue() and self.sum.hasvalue():
            self.a1.setvalue(self.sum.value - self.a2.value, self)

    def processforgetvalue(self):
        self.sum.forgetvalue(self)
        self.a1.forgetvalue(self)
        self.a2.forgetvalue(self)
        self.processnewvalue()


class multiplier:
    def __init__(self, m1, m2, product):
        self.m1 = m1
        self.m2 = m2
        self.product = product
        self.m1.connect(self)
        self.m2.connect(self)
        self.product.connect(self)

    def processnewvalue(self):
        if (self.m1.hasvalue() and self.m1.value == 0) or (self.m2.hasvalue() and self.m2.value == 0):
            self.product.setvalue(0, self)
        elif self.m1.hasvalue() and self.m2.hasvalue():
            self.product.setvalue(self.m1.value * self.m2.value, self)
        elif self.m1.hasvalue() and self.product.hasvalue():
            self.m2.setvalue(self.product.value / self.m1.value, self)
        elif self.m2.hasvalue() and self.product.hasvalue():
            self.m1.setvalue(self.product.value / self.m2.value,self)

    def processforgetvalue(self):
        self.product.forgetvalue(self)
        self.m1.forgetvalue(self)
        self.m2.forgetvalue(self)
        self.processnewvalue()


class squarer:
    def __init__(self, a, b):
        self.a = a
        self.b = b
        self.a.connect(self)
        self.b.connect(self)

    def processnewvalue(self):
        if self.b.hasvalue():
            if self.b.value < 0:
                raise Exception("square less than 0: SQUARER", self.b.value)
            else:
                self.a.setvalue(sqrt(self.b.value), self)
        elif self.a.hasvalue():
            self.b.setvalue(self.a.value ** 2, self)

    def processforgetvalue(self):
        self.a.forgetvalue(self)
        self.b.forgetvalue(self)
        self.processnewvalue()

class probe:
    def __init__(self, name, connector):
        self.name = name
        self.connector = connector
        self.connector.connect(self)

    def printprobe(self, value):
        print("\n", "Probe:", self.name, "=", value)

    def processnewvalue(self):
        self.printprobe(self.connector.value)

    def processforgetvalue(self):
        self.printprobe("?")


class constant:
    def __init__(self, value, connector):
        connector.connect(self)
        connector.setvalue(value, self)


def testfahrenheitandcelsius():
    # 9C = 5(F - 32)
    C = connector()
    F = connector()

    u = connector()
    v = connector()
    w = connector()
    x = connector()
    y = connector()

    multiplier(C, w, u)
    multiplier(v, x, u)
    adder(v, y, F)
    constant(9, w)
    constant(5, x)
    constant(32, y)

    probe("Celsius temp", C)
    probe("Fahrenheit temp", F)

    C.setvalue(25, "user")
    C.forgetvalue("user")
    F.setvalue(212, "user")


def testaverager():
    a = connector()
    b = connector()
    c = connector()

    d = connector()
    e = connector()

    adder(a, b, d)
    multiplier(e, c, d)
    constant(2, e)

    probe("addend a", a)
    probe("augend b", b)
    probe("averager c", c)

    a.setvalue(4, "ada")
    b.setvalue(6, "ada")
    a.forgetvalue("ada")
    c.setvalue(13, "ada")


def testsquarer():
    a = connector()
    b = connector()

    squarer(a, b)

    probe("sqrt", a)
    probe("squarer", b)

    a.setvalue(2, "louis")
    a.forgetvalue("louis")

    b.setvalue(2, "louis")


def testcompoundconstraints():
    def c_plus(x, y):
        z = connector()
        adder(x, y, z)
        return z

    def c_multiple(x, y):
        z = connector()
        multiplier(x, y, z)
        return z

    def cv(v):
        c = connector()
        constant(v, c)
        return c

    def c_minus(x, y):
        z = connector()
        y.value = -y.value
        adder(x, y, z)
        return z

    def c_devide(x, y):
        z = connector()
        multiplier(z, y, x)
        return z

    def celsius_fahrenheit_converter(x):
        return c_plus(c_multiple(c_devide(cv(9), cv(5)), x), cv(32))


    C = connector()
    probe("Celsius temp", C)
    
    F = celsius_fahrenheit_converter(C)
    probe("Fahrenheit temp", F)
    
    C.setvalue(25, "mac")
    C.forgetvalue("mac")
    F.setvalue(212, "mac")


if __name__ == "__main__":
    #testfahrenheitandcelsius()
    #testaverager()
    #testsquarer()

    testcompoundconstraints()
