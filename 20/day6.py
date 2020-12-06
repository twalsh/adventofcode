#!/usr/bin/python3

test_input="""abc

a
b
c

ab
ac

a
a
a
a

b""".split('\n')

f = open('input6.txt')
puzzle_data = [ line[:-1] for line in f]

def count_answers(data):
    sum = 0
    group = {}
    for line in data:
        if line == "":
            sum = sum + len(group)
            group = {} 
        else:
            for answer in line:
                if not answer in group:
                    group[answer] = 1
                else:
                    group[answer] = group[answer] + 1
            
    sum = sum + len(group)
    return sum


def count_group_answers(group, group_size):
    count = 0
    for answer in group:
    
        if group[answer] == group_size:
            count = count + 1
    return count    


def count_answers2(data):
    sum = 0
    group = {}
    group_size = 0

    for line in data:
        if line == "":
            count = count_group_answers(group,group_size)
            sum = sum + count
            group = {} 
            group_size = 0
        else:
            group_size = group_size + 1
            for answer in line:
                if not answer in group:
                    group[answer] = 1
                else:
                    group[answer] = group[answer] + 1
            
    sum = sum + count_group_answers(group,group_size)
    return sum

assert count_answers(test_input) == 11 
print('Part One: ',count_answers(puzzle_data))

assert count_answers2(test_input) == 6
print('Part Two: ',count_answers2(puzzle_data))
