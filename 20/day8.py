#!/usr/bin/python3

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
        program.append((instr,int(arg)))

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
            break
        elif i == len(program):
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
    return acc 

program = load(test_code)
assert run(program) == 5

f = open('input8.txt')

code = [ line[:-1] for line in f ]
program = load(code)
acc = run(program)

assert acc == 1521
print('Part One', acc)
