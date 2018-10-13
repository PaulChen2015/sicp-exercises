from Queues import queue
class wire:
    def __init__(self):
        self.signal = 0
        self.action_procedures = []

    def setsignal(self, sig):
        if not (self.signal == sig):
            self.signal = sig
            for a in self.action_procedures:
                a()
        return "done"

    def addaction(self, action):
        self.action_procedures.insert(0, action)
        action()


class agenda:
    def __init__(self):
        self.time = 0
        self.segments = []

    def __repr__(self):
        return "segments={0}".format(self.segments)

    def currenttime(self):
        return self.time

    def setcurrenttime(self, time):
        self.time = time

    def segments(self):
        return self.segments

    def setsegments(self, segments):
        self.segments = segments

    def firstsegment(self):
        return self.segments[0]

    def restsegment(self):
        return self.segments[1:]

    def empty(self):
        return len(self.segments) == 0

    def add(self, time, action):
        def belongsbefore(segments):
            return (len(segments) == 0) or (time < segments[0].time)
        
        def makenewseg():
            q = queue()
            q.insert(action)
            return segment(time, q)

        def addtosegments(segments):
            seg = segments[0]
            if seg.time == time:
                seg.queue.insert(action)
            else:
                rest = segments[1:]
                if belongsbefore(rest):
                    segments.insert(1, makenewseg())
                else:
                    addtosegments(rest)

        if belongsbefore(self.segments):
            self.segments.insert(0, makenewseg())
        else:
            addtosegments(self.segments)

    def removefirstitem(self):
        q = self.firstsegment().queue
        q.delete()
        if q.empty():
            self.segments.pop(0)

    def firstitem(self):
        if self.empty():
            raise Exception("Agenda is empty")
        else:
            self.setcurrenttime(self.firstsegment().time)
            return self.firstsegment().queue.front()

class segment:
    def __init__(self, time, queue):
        self.time = time
        self.queue = queue

    def __repr__(self):
        return "time={0} queue={1}".format(self.time, self.queue)


theagenda = agenda()
inverterdelay = 2
andgatedelay = 3
orgatedelay = 5

def afterdelay(delay, action):
    theagenda.add(delay + theagenda.time, action)

def logicalnot(s):
    if s == 0:
        return 1
    elif s == 1:
        return 0
    else:
        raise Exception("Invalid signal", s)

def inverter(input, output):
    def invertinput():
        newvalue = logicalnot(input.signal)
        afterdelay(inverterdelay, lambda : output.setsignal(newvalue))

    input.addaction(invertinput)
    return "ok"

def andgate(a1, a2, output):
    def andaction():
        newvalue = a1.signal & a2.signal
        afterdelay(andgatedelay, lambda : output.setsignal(newvalue))

    a1.addaction(andaction)
    a2.addaction(andaction)
    return "ok"

def orgate(a1, a2, output):
    def oraction():
        newvalue = a1.signal | a2.signal
        afterdelay(orgatedelay, lambda : output.setsignal(newvalue))

    a1.addaction(oraction)
    a2.addaction(oraction)
    return "ok"

def compound_orgate(a1, a2, output):
    io1 = wire();
    io2 = wire()
    inverter(a1, io1)
    inverter(a2, io2)
    ao = wire()
    andgate(io1, io2, ao)
    inverter(ao, output)

def propagate():
    if theagenda.empty():
        return "done"
    else:
        firstitem = theagenda.firstitem()
        firstitem()
        theagenda.removefirstitem()
        return propagate()

def probe(name, wire):
    wire.addaction(lambda : print("\n", name, "currenttime = ", theagenda.currenttime(), "New-value = ", wire.signal))


def halfadder(a, b, s, c):
    d = wire()
    e = wire()
    orgate(a, b, d)
    andgate(a, b, c)
    inverter(c, e)
    andgate(d, e, s)
    return "ok"

def fulladder(a, b, cin, sum, cout):
    s = wire()
    c1 = wire()
    c2 = wire()
    halfadder(b, cin, s, c1)
    halfadder(a, s, sum, c2)
    orgate(c1, c2, cout)
    return "ok"

def ripplecarryadder(cin, an, bn, sn, cout):
    sn = []
    pairs = list(zip(an, bn))
    cout = cin
    for a, b in pairs:
        sk = wire()
        ck = wire()
        fulladder(a, b, cout, sk, ck)
        cout = ck
        sn.append(sk)


def testhalfadder():
    input_1 = wire()
    input_2 = wire()
    sum = wire()
    carry = wire()
    
    probe("sum", sum)
    probe("carry", carry)
    print(halfadder(input_1, input_2, sum, carry))
    print(input_1.setsignal(1))
    print(propagate())
    print(input_2.setsignal(1))
    print(propagate())

def testandgate():
    a1 = wire()
    a2 = wire()
    out = wire()
    
    probe("out", out)
    print(andgate(a1, a2, out))
    print(a2.setsignal(1))
    print(propagate())
    
    print(a1.setsignal(1))
    print(propagate())
    print(a2.setsignal(0))
    print(propagate())
    
if __name__ == "__main__":
    testhalfadder()
    #testandgate()
