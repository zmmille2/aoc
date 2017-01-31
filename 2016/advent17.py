import numpy
import Queue
import md5

passcode = "awrkjxxr"
instructions = Queue.Queue()
legend = {0:"U", 1:"D", 2:"L", 3:"R"}

def is_wall(info):
    answer = []
    for item in info:
        if item in ["b", "c", "d", "e", "f"]:
            answer.append(True)
        else:
            answer.append(False)
    return answer

def walls(movement_string):
    info = md5.new(passcode + movement_string).hexdigest()[:4]
    print "walls =", info
    return is_wall(info)

def can_move(info, direction):
    return info[direction]

def make_string(x, y, path, direction):
    return str(x) + " " + str(y) + " " + path + legend[direction]

def update(x, y, direction, trail):
    if direction == 0:
        if y == 0:
            return ""
        else:
            return make_string(x, y - 1, trail, direction)
    elif direction == 1:
        if y == 3:
            return ""
        else:
            return make_string(x, y + 1, trail, direction)
    elif direction == 2:
        if x == 0:
            return ""
        else:
            return make_string(x - 1, y, trail, direction)
    elif direction == 3:
        if x == 3:
            return ""
        else:
            return make_string(x + 1, y, trail, direction)
    else:
        print "oops"

instructions.put("0 0 ")
path = ""
while path == "":
    try:
        instruction = instructions.get(True, 3)
    except:
        break
    if instruction == "":
        continue
    print "instruction =", instruction
    args = instruction.split(" ")
    x, y, curr_path = int(args[0]), int(args[1]), args[2]
    print "args =", args
    if x == 3 and y == 3:
        path = curr_path
        break
    wall_info = walls(curr_path)
    print wall_info
    for direction in xrange(4):
        if can_move(wall_info, direction):
            print x, y, direction, curr_path
            instructions.put(update(x, y, direction, curr_path))
        else:
            print "couldn't move", x, y, direction, curr_path

print path
