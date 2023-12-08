from collections import defaultdict
import util
NAME = "Camel Cards"

input = util.get_input("day07")

example = """
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483""".strip().splitlines()

FIVEOAK = 7 
FOUROAK = 6
FULL = 5
THREEOAK = 4
TWOPAIR = 3
ONEPAIR = 2
HIGH_CARD = 1
# 32T3K 765
class Hand(object):
    cardstrength = "23456789TJQKA"

    def __init__(self, s) -> None:
        self.hand = s
        self.type = self.get_type(s)
        self.cardord = self.cardorder()

    def __str__(self):
        return self.hand 
    
    def __repr__(self):
        return str(self)
    
    def cardorder(self):
        x = 0
        for c in self.hand:
            x = x*13 + self.cardstrength.find(c)
        return x 

    def get_type(self, s):
        d = defaultdict(int)
        for c in s :
            d[c]+=1 
        if len(d) == 1:
            return FIVEOAK
        if len(d) == 5: 
            return HIGH_CARD
        if len(d) == 4: 
            return ONEPAIR
        if len(d) == 2: 
            if max(d.values()) == 4:
                return FOUROAK
            else:
                return FULL
        # len(d) == 3
        if max(d.values()) == 3:
            return THREEOAK
        return TWOPAIR
        
    def __lt__(self, other):
        return self.type < other.type \
                or (self.type == other.type and self.cardord < other.cardord)
    
class Hand2(Hand):
    cardstrength = "J23456789TQKA"
    def get_type(self, s):
        njacks = len([c for c in s if c == "J"])
        if njacks >= 4: 
            return FIVEOAK
        basic_type = super().get_type(s)
        if njacks == 0:
            return basic_type
        if njacks == 3:
            if basic_type == FULL: # JJJxx
                return FIVEOAK
            else: # JJJxy
                return FOUROAK
        if njacks == 2:
            if basic_type == FULL: # JJxxx
                return FIVEOAK
            if basic_type == TWOPAIR: #JJxxy
                return FOUROAK
            else: # JJxyz
                return THREEOAK
        assert njacks == 1 
        return {HIGH_CARD: ONEPAIR,
                ONEPAIR : THREEOAK,
                TWOPAIR: FULL,
                THREEOAK: FOUROAK,
                FOUROAK:FIVEOAK
                }[basic_type]



def part(data, handcls):
    hands = [ (handcls(h),int(b)) for (h,b) in [row.split() for row in data] ]
    sum = 0
    for i, (h, bid) in enumerate(sorted(hands)):
        sum += (i+1)*bid
    return sum


part1_answer = 251106089
def part1(data):
    return part(data, Hand)


part2_answer = 249620106
def part2(data):
    return part(data, Hand2)

