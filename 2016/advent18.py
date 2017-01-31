first_row = ".^^^^^.^^^..^^^^^...^.^..^^^.^^....^.^...^^^...^^^^..^...^...^^.^.^.......^..^^...^.^.^^..^^^^^...^."
room = first_row

# 2002 is too high

def get_range(center):
    if center - 1 < 0:
        low = 0
    else:
        low = center - 1
    if center + 2 > len(first_row):
        high = len(first_row)
    else:
        high = center + 2
    return low, high

def spikes(consideration, low, high):
    if len(consideration) == 3:
        return consideration in ["^^.", ".^^", "^..", "..^"]
    elif len(consideration) == 2:
        if low == 0:
            return consideration in [".^", "^^"]
        elif high == len(first_row):
            return consideration in ["^.", "^^"]
        else:
            print "you fucked it"
    else:
        print "you really fucked it"

previous_row = first_row
row = ""

for something in xrange(len(first_row) - 1):
    room += "\n"
    for center in xrange(len(first_row)):
        low, high = get_range(center)
        consideration = previous_row[low:high]
        print consideration
        if spikes(consideration, low, high):
            row += "^"
        else:
            row += "."
        if len(row) == len(first_row):
            room += row
            previous_row = row
            row = ""

print room

total = 0
safe = 0
for row in room.split("\n"):
    if total >= 40:
        break
    for char in row:
        print char, 
        if char == ".":
            safe += 1
    total += 1

print safe
