#!/usr/bin/python

import sys
from math import *
from numpy import linspace
from scipy import interpolate
import pylab

scalefactor = 0.5 * 27.1

def f(x,y):
    return 0.5*x + sin(y)

# Prepend p to the string s until its length is at least n.
def pad(s, p, n):
    x = s
    while len(x) < n:
        x = p + x
    return x

def float2sfixed(f):
    if f < 0:
        sgn = "1"
        f = f * -1
    else:
        sgn = "0"
    if f >= 2.0:
        f = 1.999 # largest representable value
        sys.stderr.write("warning: clamped unrepresentable value\n")
    ipart = int(floor(f))
    fpart = int(round((f - ipart) * 2**10))
    istr = pad(bin(ipart)[2:], "0", 1) # remove '0b' prefix and pad out
    fstr = pad(bin(fpart)[2:], "0", 10)
    # if the string is negative, take the 2's complement
    if sgn == "1":
        # take two's complement
        n = int(istr + fstr, 2)
        nc = 2**11 - n
        return sgn + pad(bin(nc)[2:], "1", 11)
    else:
        return sgn + istr + fstr

# principal component data points
x1 = []
x2 = []
y = []

f = open(sys.argv[1], 'r')
for line in f:
    tmp = line.split()
    x1.append(float(tmp[0]))
    x2.append(float(tmp[1]))
    y.append(float(tmp[2]))

pc = interpolate.interp2d(x1, x2, y)

samplePoints = list(linspace(0, 1.875, 16)) + list(linspace(-2, -0.125, 16))
xs = list(samplePoints)
ys = list(samplePoints)

x_bin = 0
for x in xs:
    y_bin = 0
    for y in ys:
        addr = pad(bin(x_bin)[2:], '0', 5) + pad(bin(y_bin)[2:], '0', 5)
        print(float2sfixed(scalefactor * pc(x,y)))
        y_bin += 1
    x_bin += 1
