commands = reversed(open('day21.txt', 'r').read().strip().split("\n"))
password = 'fbgdceah'
answer = [7,1,6,2,7,3,0,4]

def swap(password, i, j):
    password = list(password)
    password[i], password[j] = password[j], password[i]
    return ''.join(password)

def descramble(command, password):
    args = command.split(" ")
    print args, password
    if args[0] == "swap":
        # doesn't change
        if args[1] == "position":
            return swap(password, int(args[2]), int(args[5]))
        # doesn't change
        else:
            password = password.replace(args[2], 'x')
            password = password.replace(args[5], args[2])
            password = password.replace('x'    , args[5])
            return password
    elif args[0] == "reverse":
        # doesn't change
        first, second = int(args[2]), int(args[4])
        if first == 0:
            return password[second::-1] + password[second + 1:]
        else:
            return password[:first] + password[second:first-1:-1] + password[second+1:]
    elif args[0] == "rotate":
        if args[1] == "based":
            letter = args[6]
            position = password.find(letter)
            return descramble("rotate right " + str(answer[position]) + " steps", password)
        else:
        # changed
            direction, steps = args[1], int(args[2])
            if direction == "left":
                steps = len(password) - steps
            return password[steps:] + password[:steps]
    elif args[0] == "move":
        # changed
        take, put = int(args[5]), int(args[2])
        add = password[take]
        password = password[:take] + password[take + 1:]
        return password[:put] + add + password[put:]
    else:
        print "Not implemented yet"

for command in commands:
    print "password =", password, "command =",
    password = descramble(command, password)
    print "new password =", password
