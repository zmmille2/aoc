triangles = open('day3.txt', 'r')

valid = 0

lines = triangles.read().splitlines()
print len(lines)

i = 0
while(i < len(lines)):
    first = list(map(int, lines[i].split()))
    second = list(map(int, lines[i + 1].split()))
    third = list(map(int, lines[i + 2].split()))

    # print first
    # print second
    # print third

    triangle0 = [first[0], second[0], third[0]]
    triangle1 = [first[1], second[1], third[1]]
    triangle2 = [first[2], second[2], third[2]]

    if( triangle0[0] + triangle0[1] > triangle0[2]
    and triangle0[1] + triangle0[2] > triangle0[0]
    and triangle0[2] + triangle0[0] > triangle0[1]):
        valid += 1

    if( triangle2[0] + triangle2[1] > triangle2[2]
    and triangle2[1] + triangle2[2] > triangle2[0]
    and triangle2[2] + triangle2[0] > triangle2[1]):
        valid += 1

    if( triangle1[0] + triangle1[1] > triangle1[2]
    and triangle1[1] + triangle1[2] > triangle1[0]
    and triangle1[2] + triangle1[0] > triangle1[1]):
        valid += 1

    i += 3

print valid
