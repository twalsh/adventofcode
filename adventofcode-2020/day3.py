# -*- coding: utf-8 -*-
"""
Created on Thu Dec  3 11:41:42 2020

@author: TWalsh
"""

test_input="""..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"""

test_lines = test_input.split('\n')

for line in test_lines:
    print(line)

def count_trees(lines,slope):
    width = len(lines[0])
    depth = len(lines)
    (dx,dy) = slope

    x = 0
    y = 0
    count = 0

    while 1:
        y = y + dy
        if y >= depth:
            break

        x = x + dx    
        if x >= width:
            x = x - width
    
        p = lines[y][x]        
        if p == '#':
            count = count + 1
    
    return count

print(count_trees(test_lines,(3,1)))

f = open('input3.txt')
lines = [ line.strip() for line in f ]
print(count_trees(lines,(3,1)))

slopes = [(1,1),(3,1),(5,1),(7,1),(1,2)]
product = 1
for s in slopes:
    product = product * count_trees(lines,s)

print(product)