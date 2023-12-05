import util

NAME = "Scratchcards"
example = """
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
""".strip().splitlines()

input = util.get_input("day04")


def numwinners(x):
    have = set(x.split("|")[1].strip().split())
    winners = set(x.split("|")[0].split(":")[1].strip().split())
    return len(have.intersection(winners))

def cardpoints(x):
    n = numwinners(x)
    if n < 1 :
        return 0
    return 2 ** (n-1)


part1_answer = 32001

def part1(data):
    return(sum([cardpoints(r) for r in  data]))

# 
def parse_input(data):
    return {(n+1):w for n,w in enumerate([numwinners(row) for row in data])}

part2_answer = 5037841 
def part2(data):
    wd =parse_input(data)
    result = {i:1 for i in wd.keys()}
    for i in sorted(wd.keys()):
        wins = wd[i]
        for new_card_ix in  range(i+1, i+1+wins):
            result[new_card_ix]+=result[i]
    
    return(sum(result.values()))   
