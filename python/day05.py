import util

NAME = "Seeds -> Fertilizer"
example = """
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4""".strip()

input = util.get_raw_input("day05")

class mymap(object):
    def __init__(self, data):
        #print(data)
        self.name = data[0]
        self.ranges = sorted([self.parse_range(r) for r in data[1:]])

    def parse_range(self, row):
        dest, src, length =row.split()
        return int(src),int(src) +int(length), int(dest)

    def map(self, n):
        if n < self.ranges[0][0] or \
            n >= self.ranges[-1][1]:
            return n 
        for (src, mx, d) in self.ranges:
            if n < mx:
                return (n-src) + d 
        assert False, "Should not be here"

    def rmap(self, n):
        if n < self.ranges[0][0] or \
            n >= self.ranges[-1][1]:
            return n 
        for (src, mx, d) in self.ranges:
            l = mx - src 
            if d <= n < d+l:
                return (n - d) + src 
        assert False, "Should not be here"

def seeds2(seeds):
    x = seeds 
    return [(x[2*i:2*i+2]) for i in range(int(len(x)/2))]

def is_in_range(n, seeds):
    for m,l in seeds:
        if m <= n < m+l:
            return True 
    return False


def parse(data):
    h,_,f = data.split("\n",2)
    seeds = [int(r) for r in h.split()[1:]]
    maps = [mymap(r.splitlines()) for r in f.split("\n\n")]
    return seeds, maps


def gothrough(n, maps):
    for m in maps:
        n = m.map(n)
    return n 

def gothroughrev(n, maps):
    for m in reversed(maps):
        n = m.rmap(n)
    return n 

part1_answer = 389056265
def part1(data):
    seeds, maps = parse(data)
    return(min(gothrough(s, maps) for s in seeds))

part2_answer = 137516820
def part2(data):
    seeds, maps = parse(data)
    seeds = seeds2(seeds)
    n = 0 
    maxstep = 10000000
    step = 1
    s = gothroughrev(n, maps)
    count = 0
    while not(is_in_range(s , seeds)):
        count +=1 
        while True:
            nn=n + step  
            ns = gothroughrev(nn, maps)
            if step == 1 or (nn-n) == (ns-s):
                n = nn 
                s = ns 
                step = min(step*2, maxstep)
                break
            else: 
                step = max(int(step/2),1)
    
    #print(n, step, count)
    return n 
