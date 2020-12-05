#!/usr/bin/python3

def locate(code,offset):
    if code == "":
        return offset

    upper = 2**len(code)
    pivot = upper/2
    #print(code,offset,upper,pivot)
    if code[0] == 'F' or code[0] == 'L':
        return locate(code[1:],offset)
    else:
        return locate(code[1:],offset+pivot)

def get_seat_id(code):
    row = locate(code[:7],0)
    seat = locate(code[7:],0)

    seat_id = row*8 + seat
    return seat_id

test_codes = { "FBFBBFFRLR": 357,
"BFFFBBFRRR": 567, #: row 70, column 7, seat ID 567.
"FFFBBBFRRR": 119, #row 14, column 7, seat ID 119.
"BBFFBBFRLL": 820 } # row 102, column 4, seat ID 820.

for code in test_codes:
    seat_id = get_seat_id(code)
    assert seat_id == test_codes[code]

f = open('input5.txt')

max_seat_id = 0

seat_ids = [ get_seat_id(line[:-1]) for line in f ]

print( "Part One: ", max(seat_ids))

