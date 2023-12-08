import util
import math
NAME = "Camel Cards"

input = util.get_input("day08")

example = """
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)""".strip().splitlines()

MOVEDIR = { "L":0, "R":1}

def parse_network(lines):
    return  {n[:3]:(n[7:10], n[12:15]) for n in lines}

part1_answer = 23147
def part01(data):
    instructions, network = (data[0], parse_network(data[2:]))
    print(network)
    pos = "AAA"
    n = 0
    while pos != "ZZZ":
        mov = MOVEDIR[instructions[n % len(instructions)]]
        pos = network[pos][mov]
        n+=1
    print(n)

def get_first(p, instr, nw):
    n = 0
    while p[-1] != "Z":
        ip = n % len(instr)
        p = nw[p][MOVEDIR[instr[ip]]]
        n+=1 
    return n,p

# part2_answer = 48132530481716685194553863 too high
part2_answer = 22289513667691
def part02(data):
    instructions, network = (data[0], parse_network(data[2:]))
    pos = [k for k in network.keys() if k[-1] == "A"] 
    firstdists = [get_first(p, instructions, network)[0] for p in pos]
    gcd = math.gcd(*firstdists)
    print(gcd * math.prod([int(d/gcd) for d in firstdists]))