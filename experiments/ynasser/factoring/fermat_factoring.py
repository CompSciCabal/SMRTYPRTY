#from math import sqrt, ceil, pow
import gmpy2, math, time

def my_is_sqr(n):
	return math.sqrt(n) % 1 == 0

def my_fermat_factor(n):
	a = math.ceil(math.sqrt(n))
	b2 = (a*a-n)
	while not(my_is_sqr(b2)):
		a += 1
		b2 = a*a - n

	return int(a+math.sqrt(b2)), int(a-math.sqrt(b2))
	
# code from stack overflow to test gmpy2's performance
def fermat_factor(n):
    assert n % 2 != 0  # Odd integers only

    a = gmpy2.ceil(gmpy2.sqrt(n))
    b2 = gmpy2.square(a) - n
    while not is_square(b2):
        a += 1
        b2 = gmpy2.square(a) - n

    factor1 = a + gmpy2.sqrt(b2)
    factor2 = a - gmpy2.sqrt(b2)
    return int(factor1), int(factor2) 

def is_square(n):
    root = gmpy2.sqrt(n)
    return root % 1 == 0  # '4.0' will pass, '4.1212' won't

# Conclusion: gmpy2 (numerical precision library) slows things down by a lot, but
# the precision is necessary for factoring numbers that matter ...
 
start_time = time.time()
print "Mine:", my_fermat_factor(555555555555555555555555555555555), "with duration", time.time() - start_time
start_time = time.time()
print "Internet stranger:", fermat_factor(555555555555555555555555555555555), "with duration", time.time() - start_time
