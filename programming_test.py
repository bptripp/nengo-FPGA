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
sleep(0.001)

print("Programming with " + sys.argv[1])
f = open(sys.argv[1], 'r')
for line in f:
    tmp = line.split()
    # binary characters -> binary digits
    addr = tmp[0]
    data = tmp[1]
    s.send(makeframe('\x00', binstring(addr) + binstring(data)))
    sleep(0.001)

print("Starting run...")
s.send(makeframe_START())

print("done")
