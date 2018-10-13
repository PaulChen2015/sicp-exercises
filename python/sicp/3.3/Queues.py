from sicp.lisplib import *
class queue:
    def __init__(self):
        self.data = []

    def __repr__(self):
        return "{0}".format(self.data)

    def empty(self):
        return len(self.data) == 0

    def insert(self, item):
        self.data.append(item)
        return self.data

    def delete(self):
        if self.empty():
            raise Exception("DELETE called with an empty queue")
        else:
            self.data.pop(0)
        return self.data
    def front(self):
        if self.empty():
            raise Exception("FRONT called with an empty queue")
        return self.data[0]

class deque:
    def __init__(self):
        self.data = []

    def empty(self):
        return len(self.data) == 0
    
    def __repr__(self):
        return "{0}".format(self.data)

    def frontinsert(self, item):
        self.data.insert(0, item)
        return self.data

    def rearinsert(self, item):
        self.data.append(item)
        return self.data

    def frontdelete(self):
        if self.empty():
            raise Exception("FRONTDELETE called with an empty deque")
        self.data.pop(0)
        return self.data

    def reardelete(self):
        if self.empty():
            raise Exception("REARDELETE called with an empty deque")
        self.data.pop()
        return self.data

def testqueue():
    q = queue()
    q.empty()
    q.insert(1)
    q.insert("abc")
    q.empty()
    q.delete()

def testdeque():
    dq = deque()
    dq.empty()
    dq.frontinsert(2)
    dq.frontinsert(1)
    dq.rearinsert(3)
    dq.rearinsert(4)
    dq.frontdelete()
    dq.reardelete()


if __name__ == "__main__":
    testqueue()
    testdeque()
