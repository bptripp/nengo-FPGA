#!/usr/bin/python

from socket import socket, AF_PACKET, SOCK_RAW
from time import sleep
import sys
import struct

src_addr = "\xf8\x1a\x67\x03\xa0\x72"
dst_addr = "\x00\x0a\x35\x02\x8f\xc0"
ethertype = "\x88\xb5"

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

print("Starting run...")
s.send(makeframe_START())

sleep(0.1)

print("Updating input DVs...")
# write 393216 = 0.0, 393217 = 0.0 (input bank #0 address #0 is board physical address #393216, etc.)
s.send(makeframe("\x01", "\x00\x00\x00\x00\x00\x00")) 

print("done")
