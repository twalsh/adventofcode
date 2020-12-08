#!/usr/bin/python3

import copy

test_code = """nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6""".split('\n')

def load(code):
    program = []

    for line in code:
        instr,arg = line.split(' ')
        program.append([instr,int(arg)])

    return program

def run(program):
    acc = 0
    i = 0
    loop = set() 
    
    while(1):
        if i in loop:
            break
        loop.add(i)

        (instr,arg) = program[i]

        if instr == 'nop':
            i = i + 1
        elif instr == 'acc':
            acc = acc + arg
            i = i + 1
        elif instr == 'jmp':
            i = i + arg 
    return acc 
        
def run(program):
    acc = 0
    i = 0
    loop = set() 
    
    while(1):
        if i in loop:
            exitval = 'LOOP'
            break
        elif i == len(program):
            exitval = 'TERM'
            break
        else:
            loop.add(i)

            (instr,arg) = program[i]

            if instr == 'nop':
                i = i + 1
            elif instr == 'acc':
                acc = acc + arg
                i = i + 1
            elif instr == 'jmp':
                i = i + arg 
    return (acc,exitval)

tprogram = load(test_code)
acc, exitval = run(tprogram)

assert acc == 5

f = open('input8.txt')

code = [ line[:-1] for line in f ]
program = load(code)
acc,_ = run(program)

assert acc == 1521
print('Part One', acc)

tprogram2 = tprogram.copy() 
tprogram2[-2][0] = 'nop'

acc,exitval = run(tprogram2)

assert exitval == 'TERM'

def fix_program(program):
    for i in range(len(program)):
        mprogram = copy.deepcopy(program)
        if mprogram[i][0] == 'acc':
            continue
        else:
            if mprogram[i][0] == 'nop':
                mprogram[i][0] = 'jmp'
            else:
                mprogram[i][0] = 'nop'
            acc,exitval = run(mprogram)
            if exitval == 'TERM':
                return (acc,mprogram)

acc,_ = fix_program(program)
print('Part Two:',acc)
        
            

