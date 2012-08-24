import serial
import sys

s = serial.Serial(sys.argv[1], 9600)
while True:
    char = sys.stdin.read(1)
    if not char:
        break
    
    s.write(char)
    
