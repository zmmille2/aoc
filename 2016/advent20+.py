from collections import defaultdict

# 234 too high

blocks = open("day20.txt", "r").read().strip().split("\n")
ranges = defaultdict(int)

for block in blocks:
    [x, y] = map(int, block.split("-"))
    ranges[x] = y

total = 0
last_val = 0
keys = sorted(ranges.keys())

for key in keys:
    if key > last_val + 1:
        total += (key - (last_val + 1))
    last_val = max(last_val, ranges[key])

print total
