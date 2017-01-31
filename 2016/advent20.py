from collections import defaultdict

blocks = open("day20.txt", "r").read().strip().split("\n")
ranges = defaultdict(int)

for block in blocks:
    [x, y] = map(int, block.split("-"))
    ranges[x] = y

last_val = 99999
keys = sorted(ranges.keys())

for key in keys:
    if key > last_val + 1:
        print last_val + 1
        break
    else:
        last_val = max(last_val, ranges[key])
