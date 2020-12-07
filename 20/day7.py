#!/usr/bin/python3

import re


def make_containers(data):
    contains = {}
    contained_by = {}

    rule = re.compile(r"(\d+\s)?(\w+\s\w+) bag(?:s)?")

    for line in data:
        m = rule.findall(line)
    #    print(m)
        container = m[0][1]
        contains[container] = {} 
        for content in m[1:]:
            number,colour = content
            if number:
                contains[container][colour] = int(number)
            
            if not colour in contained_by:
                contained_by[colour] = { container }
            else:
                contained_by[colour].add(container)
    return(contained_by,contains)  


def find_containers(colour,contained_by):
    containers = {} 
    if colour in contained_by:
        primary_containers = contained_by[colour] 
        containers = primary_containers.copy()
        for container_colour in primary_containers:
            containers.update(find_containers(container_colour,contained_by))
        return containers
    else:
        return {} 

def find_contents(primary_colour,contains):
    print('COLOUR',primary_colour)

    count = 0 
    if primary_colour in contains:
        for colour,num in contains[primary_colour].items():
            count = count + num 
            contents_count = find_contents(colour,contains)
            for i in range(num):
                count = count + contents_count
    return count

test_input="""light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.""".split('\n')

test_bag = 'shiny gold'

(test_contained_by,test_contents) = make_containers(test_input)

assert len(find_containers(test_bag,test_contained_by)) == 4

f = open('input7.txt')

lines = [ line[:-1] for line in f ]

(contained_by,puzzle_contents) = make_containers(lines)
print('Part One:',len(find_containers(test_bag,contained_by)))

test_count = find_contents(test_bag,test_contents)
print(test_count)
assert test_count == 32

puzzle_count = find_contents(test_bag,puzzle_contents)
print('Part Two',puzzle_count)
