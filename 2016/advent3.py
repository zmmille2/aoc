triangles = open('day3.txt', 'r')

valid = 0

for line in triangles.read().splitlines():
    sides = line.split()
    sides = list(map(int, sides))
    if( sides[0] + sides[1] > sides[2]
    and sides[1] + sides[2] > sides[0]
    and sides[2] + sides[0] > sides[1]):
        valid += 1

print valid
