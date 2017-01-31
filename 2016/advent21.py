commands = open('day21.txt', 'r').read().strip().split("\n")
password = 'abcdefgh'

def swap(password, i, j):
    password = list(password)
    password[i], password[j] = password[j], password[i]
    return ''.join(password)

def scramble(command, password):
    args = command.split(" ")
    print args
    if args[0] == "swap":
        if args[1] == "position":
            return swap(password, int(args[2]), int(args[5]))
        else:
            password = password.replace(args[2], 'x')
            password = password.replace(args[5], args[2])
            password = password.replace('x'    , args[5])
            return password
    elif args[0] == "reverse":
        first, second = int(args[2]), int(args[4])
        if first == 0:
            return password[second::-1] + password[second + 1:]
        else:
            return password[:first] + password[second:first-1:-1] + password[second+1:]
    elif args[0] == "rotate":
        if args[1] == "based":
            letter = args[6]
            index = password.find(letter)
            if index >= 4:
                index += 2
            else:
                index += 1
            return scramble("rotate right " +  str(index) + " this part isn't used LOL", password)
        else:
            direction, steps = args[1], int(args[2])
            if direction == "right":
                steps = len(password) - steps
            return password[steps:] + password[:steps]
    elif args[0] == "move":
        take, put = int(args[2]), int(args[5])
        add = password[take]
        password = password[:take] + password[take + 1:]
        return password[:put] + add + password[put:]
    else:
        print "Not implemented yet"

for command in commands:
    print "password =", password, "command =",
    password = scramble(command, password)
    print "new password =", password

print password
