import pdb
import Queue
from collections import defaultdict

# 7 too low, 137 too low, 143 not right, 280 too high
offset = 1358
end_x, end_y = 31, 39

tree = Queue.Queue()
visited = defaultdict(int)
distinct_50s = 0

def is_wall(x, y):
    if x < 0 or y < 0:
        return True

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
    if length > 50 or start_x < 0 or start_y < 0:
        return

    visited[(start_x, start_y)] = length

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
    try:
        args = tree.get(True, 5)
        BST(args[0], args[1], args[2])
    except:
        break

for y in xrange(30):
    for x in xrange(30):
        if is_wall(x, y) and visited[(x, y)] != 0:
            print 'X',
        elif is_wall(x, y):
            print u"\u2588",
        elif visited[(x, y)] != 0:
            distinct_50s += 1
            if visited[(x, y)] == 50:
                print '\x1b[6;30;42m' + '.' + '\x1b[0m',
            else:
                print ".",
        else:
            print " ",
    print

print distinct_50s
