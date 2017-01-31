from collections import defaultdict
import Queue

node_files = open("day22.txt", "r").read().strip().split("\n")[2:]
nodes = defaultdict(lambda : defaultdict(Node))

def read_info(info):
    necessities = filter(None, info.split(" "))
    [x, y] = necessities[0].replace("/dev/grid/node-", "").split("-")
    x = int(x.replace("x", ""))
    y = int(y.replace("y", ""))
    size      = int(necessities[1].replace("T", ""))
    used      = int(necessities[2].replace("T", ""))
    available = int(necessities[3].replace("T", ""))
    use       = int(necessities[4].replace("%", ""))

    return x, y, size, used, available, use

class Node():
    def __init__(self, string):
        self.x, self.y, self.size, self.used, self.available, self.use = read_info(string)
        self.is_goal = False

    def is_empty(self):
        return self.used == 0

    def print_info(self):
        print "X\tY\tSize\tUsed\tAvail\tUse%\tIs goal?"
        print str(self.x) + "\t" + str(self.y) + "\t" + str(self.size) + "\t",
        print str(self.used) + "\t" +str(self.available) + "\t" + str(self.use),
        print "\t" + str(self.is_goal)

for node_file in node_files:
    node = Node(node_file)
    nodes[node.x][node.y] = node

finish = nodes[0][0]
goal   = nodes[29][0]
goal.is_goal = True

moves = Queue.Queue()

for col in nodes.keys():
    print
    for row in nodes[col].keys():
        node = nodes[col][row]
        if node.size > 100:
            print "#",
        elif node.x == 0 and node.y == 0:
            print "O",
        elif node.used == 0:
            print "_",
        elif node.is_goal:
            print "G",
        else:
            print ".",

# 192 too high
