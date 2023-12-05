import os 

def get_input(day):
    with open(os.path.join("../input/","{}.txt".format(day))) as f:
        return f.read().strip().splitlines()

def get_raw_input(day):
    with open(os.path.join("../input/","{}.txt".format(day))) as f:
        return f.read().strip()
