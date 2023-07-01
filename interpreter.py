# You need python >= 3.10 to run this script
# Usage: python3.10 interpreter.py <path to wee file>
import sys

lines = open(sys.argv[1]).readlines()

mem = [0] * 100000
A = B = pc = 0

while True:
    instruction = lines[pc].split()
    match(instruction[0].lower()):
        case 'mov':     A = int(instruction[1])
        case 'swap':    A, B = B, A
        case 'add':     A += B
        case 'sub':     A -= B
        case 'load':    A = mem[A]
        case 'store':   mem[A] = B
        case 'setlt':   A = A < B
        case 'getc':    A = ord(sys.stdin.read(1))
        case 'putc':    print(chr(A), end='', flush=True)
        case 'exit':    sys.exit(0)
        case 'jmpz':
            if A == 0:
                pc = int(instruction[1])
                continue
    pc += 1
