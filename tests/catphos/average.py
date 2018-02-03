#!/usr/bin/python
 
import numpy
from numpy import genfromtxt
from sys import stdin

d = genfromtxt(stdin, delimiter=',', skip_header=True, dtype=float)
print(numpy.average(d))
