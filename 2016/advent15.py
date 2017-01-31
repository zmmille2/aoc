disc_info = open("day15.txt", "r").read().strip().split("\n")
discs = []

def get_disc(info):
    words = info.split(" ")
    num = words[1].replace("#", "")
    position = words[-1].replace(".", "")
    return [int(num), int(words[3]), int(position)]

def lineup(holes, i):
    for hole in holes:
        if (hole[2] + i) % hole[1] != 0:
            return False

    return True

for disc in disc_info:
    discs.append(get_disc(disc))

for disc in discs:
    disc[2] = ((disc[2] + disc[0]) % disc[1])

for index in xrange(10000000000):
    if lineup(discs, index):
        print index
