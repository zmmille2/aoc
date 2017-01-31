from collections import defaultdict
import sys
import operator

def split(room):
    room = room.replace("-", "")
    room = room.replace("[", "")
    room = room.replace("]", "")

    name, sector, checksum = "", "", ""
    nameFull = False
    for char in room:
        if char.isalpha():
            if nameFull:
                checksum += char
            else:
                name += char
        elif char.isdigit():
            nameFull = True
            sector += char
        else: # we got a space or something
            pass
            # print "wtf"
    return name, sector, checksum

# not just 5, but the 5 most common
def isReal(name, checksum):
    frequencies = defaultdict(int)

    for char in name:
        frequencies[char] += 1

    for item in frequencies.keys():
        print item,
        print frequencies[item],

    print

    sf = sorted(frequencies.items(), key=operator.itemgetter(1))
    sf.reverse()

    for first in xrange(len(sf)):
        for second in xrange(len(sf)):
            if sf[first][1] == sf[second][1]:
                if sf[first][0] < sf[second][0]:
                    sf[first], sf[second] = sf[second], sf[first]

    answer = ''.join([letter[0] for letter in sf[:5]])

    print "checksum",
    print checksum

    print "answer",
    print answer

    print '---'

    return answer == checksum

rooms = open("day4.txt", 'r')
sector_sum = 0

for room in rooms.read().strip().split("\n"):
    name, sector, checksum = split(room)
    if(isReal(name, checksum)):
        sector_sum += int(sector)
        print '---'
        print sector_sum

print sector_sum

rooms.close()
    #print split(room)
