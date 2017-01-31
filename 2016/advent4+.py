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

def rotate(char, sector):
    num = ord(char) - ord('a')
    return chr(((num + int(sector)) % 26) + ord('a'))

rooms = open("day4.txt", 'r')
new_room = ""

for room in rooms.read().strip().split("\n"):
    new_room = ""
    name, sector, checksum = split(room)
    for char in room:
        new_room += rotate(char, sector)
    if(new_room.startswith("northpole")):
        print sector
    print new_room 
