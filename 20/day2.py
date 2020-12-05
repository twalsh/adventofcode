# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import re

rex = re.compile(r'(\d+)-(\d+) (\w): (\w+)')

test_input = ["1-3 a: abcde",
              "1-3 b: cdefg",
              "2-9 c: ccccccccc"]


def is_valid(line):
    print(line)
    m = rex.match(line)

    (low,high) = (int(m.group(1)), int(m.group(2)))
    (letter,passwd) = (m.group(3), m.group(4))
    
    count = passwd.count(letter)
    
    if count >= low and count <= high:
        return True
    else:
        return False
    
for line in test_input:
    print(line,is_valid(line))

f = open('input2.txt')

valid_count = 0
for line in f:
    if is_valid(line):
        valid_count = valid_count + 1
        
print(valid_count)

def is_valid2(line):
    #print(line)
    m = rex.match(line)

    (p,q) = (int(m.group(1))-1, int(m.group(2))-1)
    (letter,passwd) = (m.group(3), m.group(4))
    
    if passwd[p] == letter and passwd[q] == letter:
        return False
    elif passwd[p] == letter and passwd[q] != letter:
        return True
    elif passwd[p] != letter and passwd[q] == letter:
        return True
    else:
        return False
    
for line in test_input:
    print(line,is_valid2(line))
    
f = open('input2.txt')

valid_count = 0
for line in f:
    if is_valid2(line):
        valid_count = valid_count + 1
        
print(valid_count)