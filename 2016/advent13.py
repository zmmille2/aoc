import Queue
from collections import defaultdict

offset = 1358
end_x, end_y = 31, 39

tree = Queue.Queue()
visited = defaultdict(int)

def is_wall(x, y):
    binomial = x*x + 3*x + 2*x*y + y + y*y
    binomial += offset
    binary = bin(binomial)

    odd, b = 0, False
    for bit in binary:
        if not b:
            if bit == 'b':
                b = True
        else:
            odd = int(odd != int(bit))

    return odd == 1

def BST(start_x, start_y, length):
    # print "x =", start_x, "y =", start_y, "length =", length
    visited[(start_x, start_y)] = 1

    if start_x == end_x and start_y == end_y:
        return length

    if (start_x % 10 == 0 and start_y % 10 == 0) or length % 10 == 0:
        print "x =", start_x, "y =", start_y, "length =", length

    if not (is_wall(start_x - 1, start_y) or
           visited[(start_x - 1, start_y)]):
          tree.put([start_x - 1, start_y, length + 1])

    if not (is_wall(start_x + 1, start_y) or
           visited[(start_x + 1, start_y)]):
          tree.put([start_x + 1, start_y, length + 1])

    if not (is_wall(start_x, start_y - 1) or
           visited[(start_x, start_y - 1)]):
          tree.put([start_x, start_y - 1, length + 1])

    if not (is_wall(start_x, start_y + 1) or
           visited[(start_x, start_y + 1)]):
          tree.put([start_x, start_y + 1, length + 1])

tree.put([1, 1, 0])
while tree.not_empty:
    args = tree.get(True, 5)
    answer = BST(args[0], args[1], args[2])
    if answer != None:
        print answer
