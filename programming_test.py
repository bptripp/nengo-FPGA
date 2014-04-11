#!/usr/bin/python

from socket import socket, AF_PACKET, SOCK_RAW
from time import sleep
import sys
import struct
import math

src_addr = "\xf8\x1a\x67\x03\xa0\x72"
dst_addr = "\x00\x0a\x35\x02\x8f\xc0"
ethertype = "\x88\xb5"

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
    if f > 2047.0 / 1024.0:
        f = 2047.0 / 1024.0 # largest representable value
    ipart = int(math.floor(f))
    fpart = int(round((f - ipart) * 2**10))
    istr = pad(bin(ipart)[2:], "0", 1) # remove '0b' prefix and pad out
    fstr = pad(bin(fpart)[2:], "0", 10)
    # if the string is negative, take the 2's complement
    if sgn == "1":
        # take two's complement
        n = int(istr + fstr, 2)
        nc = 2**12 - n
        return pad(bin(nc)[2:], "1", 12)
    else:
        return sgn + istr + fstr

def makeframe(opcode, data):
    tag = "\x42" # FIXME better tag handling
    frame = dst_addr + src_addr + ethertype + tag + opcode + data
    # padding
    while len(frame) < 60:
        frame += "\x00"    
    return frame

def makeframe_RESET():
    return makeframe("\xFF", "")

def makeframe_START():
    return makeframe("\xFE", "")

def makeframe_SINGLESTEP():
    return makeframe("\xFD", "")

def makeframe_PAUSE():
    return makeframe("\xFC", "")

def binstring(s):
    retval = ""
    rest = s
    while(len(rest) > 0):
        first = rest[0:8]
        rest = rest[8:]
        retval += chr(int(first, 2))        
    return retval

s = socket(AF_PACKET, SOCK_RAW)
s.bind(("eth1", 0))

print("Resetting...")
s.send(makeframe_RESET())
sleep(0.1)

print("Programming with " + sys.argv[1])
f = open(sys.argv[1], 'r')
for line in f:
    tmp = line.split()
    # binary characters -> binary digits
    addr = tmp[0]
    data = tmp[1]
    s.send(makeframe('\x00', binstring(addr) + binstring(data)))
    #sleep(0.001)

sleep(1.0)

print("Starting run...")
s.send(makeframe_START())

# DV input frame: |op = 0x01|addr|addr|pair count|data|data|data|...
# write to (393216, 393217) (input bank #0 address #0 is board physical address #393216, etc.)

# 0.0: (0.0, 1.0)
# 1.5: (0.0, 0.8)
# 2.5: (0.5, 0.8)
# 3.0: (0.5, 0.0)
# 4.0: (-0.5, 0.0)
# 4.5: (-0.5, 0.8)

s.send(makeframe("\x01", "\x00\x00\x00" + "\x00\x04\x00")) 
sleep(1.5)
# t = 1.5
s.send(makeframe("\x01", "\x00\x00\x00" + "\x00\x02\x66")) 
sleep(1.0)
# t = 2.5
s.send(makeframe("\x01", "\x00\x00\x00" + "\x20\x02\x66")) 
sleep(0.5)
# t = 3.0
s.send(makeframe("\x01", "\x00\x00\x00" + "\x20\x00\x00")) 
sleep(1.0)
# t = 4.0
s.send(makeframe("\x01", "\x00\x00\x00" + "\xe0\x00\x00")) 
sleep(0.5)
# t = 4.5
s.send(makeframe("\x01", "\x00\x00\x00" + "\xe0\x02\x66")) 

print("done")
