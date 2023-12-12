import util
import math
NAME = "Mirage"

input = util.get_input("day09")

example = """
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45""".strip().splitlines()


def diffs(x):
    return [j-i for i, j in zip(x[:-1], x[1:])]

def predict_next(v):
    if all([i == 0 for i in v]):
        return 0 
    return v[-1] + predict_next(diffs(v))


part1_answer = 1898776583
def part1(data):
    values = [predict_next([int(r) for r in row.split()]) for row in data]
    return (sum(values))

part2_answer = 1100
def part2(data):
    values = [predict_next(list(reversed([int(r) for r in row.split()]))) for row in data]
    return (sum(values))


