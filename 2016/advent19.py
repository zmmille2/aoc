from collections import defaultdict

num_elves = 3001330

def next_elf(key):
    key += 1
    key %= num_elves
    while elves.get(key) == None or elves.get(key) == 0:
        key += 1
        key %= num_elves
    return key

elves = {}
for elf in xrange(num_elves):
    elves[elf] = 1

curr_elf = 0
while len(elves) != 1:
    value = elves.get(curr_elf)
    if value == 0:
        pass
    else:
        sucker = next_elf(curr_elf)
        if sucker == curr_elf:
            break
        elves[curr_elf] += elves[sucker]
        elves[sucker] = 0
    curr_elf = next_elf(curr_elf)

print [x for x in elves.keys() if elves[x] != 0]
