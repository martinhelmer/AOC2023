import util
import math
import queue

NAME = "Pipe Maze"

input = util.get_input("day10")

example = """
..F7.
.FJ|.
SJ.L7
|F--J
LJ...""".strip().splitlines()

EW = "-"
NS = "|"
NE = "L"
ES = "F"
SW = "7"
WN = "J"

NORTH = (-1,0)
EAST = (0,1)
SOUTH = (1,0)
WEST = (0,-1)

def indexes(d):
    return ((r,c) for r in range(len(d)) for c in range(len(d[0]))) 


def get_start_dir(data, spoint):
    r,c = spoint
    if c < len(data[0]):
        # try right
        if data[r][c+1] in [EW, SW, WN]:
            return EAST
    if r > 0 : 
        # try up 
        if data[r-1][c] in [NS, ES, SW]:
            return NORTH
    if c > 0:
        # try left
        if data[r][c-1] in [EW, NE, ES]:
            return WEST
        
    print("Fail to get first move")

def turn(currdir, c):
    try:
        if currdir == NORTH:
            return { "|" : NORTH,
                    "F" : EAST,
                    "7" : WEST}[c]
        if currdir == EAST:
            return { "-" : EAST,
                    "7" : SOUTH,
                    "J" : NORTH}[c]
        if currdir == SOUTH:
            return { "|" : SOUTH,
                    "J" : WEST,
                    "L" : EAST}[c]
        if currdir == WEST:
            return { "-" : WEST,
                    "F" : SOUTH,
                    "L" : NORTH}[c]   
    except KeyError:
        return currdir

def left(dir):
    return {NORTH:WEST,
            EAST:NORTH,
            SOUTH:EAST,
            WEST:SOUTH}[dir]

def right(dir):
    return {NORTH:EAST,
            EAST:SOUTH,
            SOUTH:WEST,
            WEST:NORTH}[dir]

def is_valid_pos(pos,data):
    return pos[0] >= 0 and pos[1] >= 0 and pos[0] < len(data) and pos[1] < len(data[0])
                                                                            
def do_sides(data, pos, face_dir, f, fc):
    side_pos = move(pos, f(face_dir))
    if not is_valid_pos(side_pos, data) or data[side_pos[0]][side_pos[1]] != '.':
        return 
    fill(data, side_pos, fc)

def fill(data, p, fc):
    q = queue.SimpleQueue()
    q.put(p)
    while not q.empty():
        p = q.get()
        if is_valid_pos(p, data) and data[p[0]][p[1]] == '.':
            data[p[0]][p[1]] = fc
            for d in [NORTH, EAST, SOUTH, WEST]:
                np = move(p, d)
                q.put(np)

def do_initial_loop(data):
    start_pos = next(((r,c) for (r,c) in indexes(data) if data[r][c] == 'S'))
    start_dir = get_start_dir(data, start_pos)

    this_pos = move(start_pos, start_dir)
    this_dir = start_dir
    nmoves = 1
    pp=[(this_pos, this_dir)]
    while this_pos != start_pos:
        this_dir = turn(this_dir, data[this_pos[0]][this_pos[1]])
        this_pos = move(this_pos, this_dir)
        nmoves +=1 
        pp.append((this_pos, this_dir))
    return pp, nmoves     

def datastr(data):
    return "\n".join("".join(r for r in row) for row in data)

#  7005
part1_answer = 7005
def part1(data):
    data = [list(row) for row in data]
    nd, nmoves = do_initial_loop(data)
    return(int(nmoves/2))

def move(pos, dir):
    x1,y1 = pos 
    x2, y2 = dir
    return (x1+x2, y1+y2)

def day01b(data):
     data = [list(row) for row in data]
    #  print(datastr(data))
     print(nmoves)

part2_answer = 417
def part2(data):
    ixs = indexes(data)
    data = [list(row) for row in data]
    pp, nmoves = do_initial_loop(data)
    pp_points = set([p[0] for p in pp])
    for p in ixs:
        if p not in pp_points:
            r,c  = p 
            data[r][c] = '.'
    #print(datastr(data))
    for (p,d) in pp:
        #do_sides(data, p,d,left, "O")
        do_sides(data, p,d,right, "X")
        #do_sides(data, p, turn(d, data[p[0]][p[1]]), left, "O")
        do_sides(data, p, turn(d, data[p[0]][p[1]]), right, "X")
    print(datastr(data))
    return(len([i for i in datastr(data) if i=="X"]))
    