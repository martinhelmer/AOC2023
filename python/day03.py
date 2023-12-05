import util
from collections import defaultdict
example = """
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..""".strip().split()
NAME = "Gear Ratios"

input = util.get_input("day03")

ON_NUMBER = 1
OFF_NUMBER = 0 

engine = example 

def get_context(matrix, indexes):
    l = []
    for (r,c) in indexes:
        if r < 0 or c < 0 :
            continue 
        try:
          v = matrix[r][c]
        except IndexError:
            continue 
        if v != "." and not v.isdigit():
            l.append((v,(r,c)))
    return l 

def parse_neighbors(engine):
    numbers = []
    for rix, row in enumerate(engine):
        state = OFF_NUMBER
        stuffaround = []
        this_number = 0
        for cix, c in enumerate(row):
            if state == OFF_NUMBER:
                if not c.isdigit():
                    continue
                state = ON_NUMBER
                this_number = int(c)
                stuffaround.extend(get_context(engine, 
                                            [(rix-1,cix-1),
                                                (rix-1,cix),
                                                (rix, cix-1),
                                                (rix+1,cix-1),
                                                (rix+1, cix)]))
                continue 
            if state == ON_NUMBER:
                if c.isdigit():
                    this_number = this_number * 10 + int(c)
                    stuffaround.extend(get_context(engine, 
                                    [(rix-1,cix),
                                    (rix+1, cix)]))
                                
                    continue 
                state = OFF_NUMBER
                stuffaround.extend(get_context(engine, 
                                [(rix-1,cix),
                                (rix, cix),
                                (rix+1, cix)]))            
                numbers.append((this_number, stuffaround))
                this_number = 0 
                stuffaround = []
        if state == ON_NUMBER:           
                numbers.append((this_number, stuffaround)) 
    return numbers 

part1_answer = 544664

def part1(engine):
    part_numbers = [a for a,b in parse_neighbors(engine) if b]
    return(sum(part_numbers))


part2_answer = 84495585
def part2(engine):
    pn = parse_neighbors(engine) 
    d = defaultdict(list)
    for num, parts in pn :
        for part in parts:
            #print(part)
            t,c = part
            if t == "*":
                d[part].append(num)
    gear_ratios = []
    for v in d.values():
        if len(v) == 2:
            gear_ratios.append(v[0]*v[1])
    return(sum(gear_ratios))
