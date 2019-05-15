#!/usr/bin/python3

import sys

while True:
	c = sys.stdin.read(1)

	if 0 == len(c): 
		break

	print ('.BYTE $%02X' % (ord(c)))
