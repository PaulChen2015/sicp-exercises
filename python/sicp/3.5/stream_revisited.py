def divisible(x, n):
	return (x % n) == 0
	
def int_from(n):
	while True:
		yield n
		n += 1
		

def sieve(stream):
	n = next(stream)
	yield n
	s = sieve(filter((lambda x : not (divisible(x, n))), stream))
	for m in s:
		yield m
		

# 会改变原来的数据
#def stream_ref(stream, n):
#	while n > 0:
#		next(stream)
#		n -= 1
#	return next(stream)
