#!/usr/bin/python3

import sys

test_input = [ int(n) for n in  """35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576""".split('\n') ]

preamble_length_t = 5

def find_number(data,preamble_length):
    for i in range(preamble_length,len(data)):
        n = data[i]
        preamble = data[i-preamble_length:i]

        is_sum = False
        for a in preamble:
            for b in preamble:
                if n == a + b:
                    is_sum = True
                    break
            if is_sum == True:
                break 
        if not is_sum:
            return n
    

test_n = find_number(test_input,preamble_length_t)
assert test_n == 127

# Part One 

f = open('input9.txt')
data = [ int(line) for line in f ]

invalid_number = find_number(data,25)
print('Day 9, Part One:', invalid_number)
assert invalid_number == 1124361034 

# Part Two

def find_set(data,n):
    for i in range(len(data)-2):
        for j in range(i,len(data)-1):    
            test_set = data[i:j]
            test_sum = sum(test_set) 
            if test_sum == n: 
                return test_set 
            elif test_sum > n:
                break 

def encryption_weakness(number_set):
    return min(number_set) + max(number_set)

test_set = find_set(test_input,127)
test_weak = encryption_weakness(test_set)
assert test_weak == 62

puzzle_set = find_set(data,invalid_number)
puzzle_weak = encryption_weakness(puzzle_set)
print('Day 9, Part Two:', puzzle_weak)
