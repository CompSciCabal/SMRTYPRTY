import sys
sys.setrecursionlimit(2000000000)

def factoring(n, lst, count):
	if (count > n):
		print "The factors are", lst
	elif (n % count == 0):
		lst.append(count)
		print "list is", lst
		factoring(n, lst, count+1)
	else:
		factoring(n, lst, count+1)

k = [1]
factoring(472822,k,2)