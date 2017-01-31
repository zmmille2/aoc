node_files = open("day22.txt", "r").read().strip().split("\n")[2:]
nodes = []
# 567 too low

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

    def is_empty(self):
        return self.used == 0

    def print_info(self):
        print "X, Y, Size, Used, Avail, Use%"
        print self.x, self.y, self.size, self.used, self.available, self.use

valid = 0

for node_file in node_files:
    nodes.append(Node(node_file))

for A in nodes:
    if not A.is_empty():
        for B in nodes:
            if A.used <= B.available:
                valid += 1

print valid
