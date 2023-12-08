import math
NAME = "Boat Race"
input = """
Time:        42     89     91     89
Distance:   308   1170   1291   1467""".strip().splitlines()




#
# (T - x)x = D
# xx - TX + D = 0 
# x = T +- sqrt(TT - 4D) 

def win_interval(T, D):
    q = math.sqrt(T**2 - 4*D)
    s1 = (T-q) / 2
    s2 = (T+q) / 2
    return int(s1)+1, int(s2) if s2>int(s2) else int(s2)-1

def numways(q):
    return q[1]-q[0]+1 

part1_answer = 3317888
def part1(_):
    parsed_input = [(42,308),(89,1170),(91,1291),(89,1467)]
    return(math.prod(numways(win_interval(T,D)) for T,D in parsed_input))

part2_answer = 24655068
def part2(_):
    T = 42899189
    D = 308117012911467
    return(numways(win_interval(T,D)))