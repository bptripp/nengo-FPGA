#!/usr/bin/python3

import matplotlib.pyplot as plt
import sys

dataPoints = []

if len(sys.argv) != 2:
    print("Usage: " + sys.argv[0] + " dumpfile")
    exit()

for line in open(sys.argv[1]):
    i = 0
    while len(line) > 0:
        valStr = line[0:3].strip()
        if len(valStr) == 0:
            break
        val = int(valStr, 16)
        if val > 2047:
            # 2's complement
            val = -1 * (2**12 - val)
        if len(dataPoints) <= i:
            dataPoints.append([])
        # convert to sfixed
        val *= 2**-10
        dataPoints[i].append(val)
        line = line[3:]
        i += 1

for pts in dataPoints:
    plt.plot(pts)

plt.show()
