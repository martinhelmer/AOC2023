
import day02 
import day03
import day04
import day05 

import time 

modules = [day02, day03, day04, day05]
for d in modules:
    s = time.time()
    v = d.part1(d.input)
    p1t = (time.time() - s)*1000 
    assert v == d.part1_answer
    s = time.time()
    v = d.part2(d.input)
    p2t = (time.time() - s) * 1000
    assert v == d.part2_answer
    l = "{} ({:>6.2f})".format( d.part1_answer, p1t)
    r = "{} ({:>6.2f})".format(d.part2_answer, p2t)
    print ("{} {:>20}: {:>20} | {:>20}".format(d.__name__, d.NAME, l,r))