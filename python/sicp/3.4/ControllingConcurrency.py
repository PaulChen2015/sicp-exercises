from concurrent import futures
import random
import time

class c:
    def __init__(self, value):
        c.value = value

x = c(10)
def square(n):
    time.sleep(random.uniform(0, 1))
    n.value = n.value ** 2
def inc(n):
    time.sleep(random.uniform(0, 1))
    n.value = n.value + 1
with futures.ThreadPoolExecutor(max_workers=2) as executor:
    executor.submit(inc, x)
    executor.submit(square, x)
    
print(x.value)
