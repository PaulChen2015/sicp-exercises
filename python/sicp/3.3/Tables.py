from sicp.lisplib import *
class node:
    def __init__(self, key, value, left, right):
        self.key = key
        self.value = value
        self.left = left
        self.right = right

    def __repr__(self):
        return "[key=[{0}] value=[{1}] left=[{2}] right=[{3}]]".format(self.key, self.value, self.left, self.right)

    def setvalue(self, v):
        self.value = v

    def setleft(self, n):
        self.left = n

    def setright(self, n):
        self.right = n


class table:
    def __init__(self):
        self.data = []

    def assoc(self, k):
        for e in self.data:
            if k == e[0]:
                return e[1]

        return None

    def get(self, k1, k2):
        node = self.assoc(k1)
        if node:
            return self.getvalue(k2, node)
        return None            

    def getvalue(self, k, node):
        if node is None:
            return None

        if k == node.key:
            return node.value
        elif k < node.key:
            return self.getvalue(k, node.left)
        else:
            return self.getvalue(k, node.right)

    def put(self, k1, k2, v):
        e = self.assoc(k1)
        if e:
            self.putvalue(k2, v, e)
        else:
            self.data.append(cons(k1, node(k2, v, None, None)))

    def putvalue(self, k, v, e):
        if k == e.key:
            e.setvalue(v)
        elif k < e.key:
            l = e.left
            if l is None:
                e.setleft(node(k, v, None, None))
            else:
                return self.putvalue(k, v, l)
        else:
            r = e.right
            if r is None:
                e.setright(node(k, v, None, None))
            else:
                return self.putvalue(k, v, r)



if __name__ == "__main__":
    t = table()
    t.put("math", "+", 43)
    t.put("math", "-", 45)
    t.put("math", "*", 42)
    t.put("letters", "c", 99)
    t.put("letters", "a", 97)
    t.put("letters", "b", 98)
    t.put("letters", "d", 100)
    
