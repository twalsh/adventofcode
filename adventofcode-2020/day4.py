import re
import sys

test_input = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in"""

# Part One

def read_passports(lines):
    passports = []
    passport = {}

    for line in lines:
        if line == "": 
        #print('#'+line+'#')
        
            passports.append(passport)
            passport = {}
        else:
            fields = line.split(' ')
            for field in fields:
                key,value = field.split(':')
                passport[key] = value

    return passports

required_fields = set(['byr',
'iyr',
'eyr', 
'hgt', 
'hcl', 
'ecl', 
'pid'])

def is_valid_passport(passport):
    for field in required_fields:
        if not field in passport:
            #print(field + ' missing')
            return False
    return True



test_lines = test_input.split('\n')

test_passports = read_passports(test_lines)

def count_valid_passports(passports,is_valid):
    valid_passports = [ p for p in passports if is_valid(p) ]
    return len(valid_passports)

print(count_valid_passports(test_passports,is_valid_passport))

f = open('input4.txt')
puzzle_input = [ line[:-1] for line in f ]

passports = read_passports(puzzle_input)
print('Part One:',count_valid_passports(passports,is_valid_passport))

# Part Two

hcl_re = re.compile(r'^#[0-9a-f]{6}$')
pid_re = re.compile(r'^[0-9]{9}$')

def is_valid_passport2(passport):
    
    for field in required_fields:
        if not field in passport:
            return False
        else:
            value = passport[field]
            
            if field == 'byr':
                year = int(value)
                if not (year >= 1920 and year <= 2002):
                    return False
            elif field == 'iyr':
                year = int(value)
                if not (year >= 2010 and year <= 2020):
                    return False
            
            elif field == 'eyr':
                year = int(value)
                if not (year >= 2020 and year <= 2030):
                    return False
            elif field == 'hgt':
                unit = value[-2:]
                if unit != 'cm' and unit != 'in':
                    return False
            
                height = int(value[:-2])
                
                if unit == 'cm' and (height >= 150 and height <= 193):
                    pass
                elif unit == 'in' and (height >= 59 and height <= 76):
                    pass
                else:
                    return False
            elif field == 'hcl':
                if not hcl_re.match(value):
                    return False
            elif field == 'ecl':
                if not value in {'amb','blu','brn','gry','grn','hzl','oth'}:
                    return False
            elif field == 'pid':
                if not pid_re.match(value):
                    return False
    return True

# Invalid test passports

invalid_lines = """eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007""".split('\n')

invalid_passports = read_passports(invalid_lines)
assert count_valid_passports(invalid_passports,is_valid_passport2) == 0

# Valid test passports

valid_lines="""pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:09315471""".split('\n')

valid_passports = read_passports(valid_lines)
assert count_valid_passports(valid_passports,is_valid_passport2) == len(valid_passports)
print('Valid test passports OK')
valid_puzzle_passports = count_valid_passports(passports,is_valid_passport2)
print('Part Two:',valid_puzzle_passports)
