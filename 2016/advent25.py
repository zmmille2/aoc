instructions = open("day25.txt", "r").read().strip().split("\n")

def is_int(string):
    try:
        int(string)
        return True
    except ValueError:
        return False

def eval(instructions, environment, i):
    instruction = instructions[i].split(" ")
    command = instruction[0]
    arg1 = instruction[1]
    if len(instruction) == 3:
        arg2 = instruction[2]

    if command == "out":
        if is_int(arg1):
            output.append(arg1)
            print arg1,
        else:
            output.append(env[arg1])
            print env[arg1],

    if command == "cpy":
        if is_int(arg1):
            environment[arg2] = int(arg1)
        elif is_int(arg2):
            return instructions, environment, i + 1
        else:
            environment[arg2] = environment[arg1]
        return instructions, environment, i + 1

    elif command == "inc":
        if is_int(arg1):
            return instructions, environment, i + 1
        environment[arg1] += 1
        return instructions, environment, i + 1

    elif command == "dec":
        if is_int(arg1):
            return instructions, environment, i + 1
        environment[arg1] -= 1
        return instructions, environment, i + 1

    elif command == "jnz":
        if not is_int(arg2):
            arg2 = environment[arg2]
        if is_int(arg1):
            if int(arg1) != 0:
                return instructions, environment, i + int(arg2)
        elif environment[arg1] != 0:
            return instructions, environment, i + int(arg2)
        return instructions, environment, i + 1

    elif command == "tgl":
        if is_int(arg1):
            target = i + arg1
        else:
            target = i + env[arg1]
        if target < 0 or target >= len(instructions):
            return instructions, environment, i + 1
        toggled = instructions[target].split(" ")
        update = toggled[0]
        arg1 = toggled[1]
        if len(toggled) == 3:
            arg2 = toggled[2]
            if update == "jnz":
                update = "cpy"
            else:
                update = "jnz"
            instructions[target] = ' '.join([update, arg1, arg2])
        else:
            if update == "inc":
                update = "dec"
            else:
                update = "inc"
            instructions[target] = ' '.join([update, arg1])

        return instructions, environment, index + 1
    else:
        return instructions, environment, index + 1

output, integer, index = [], 0, 0
env = {"a": integer}
while output != [1, 0] * 10:
    while index < len(instructions) and len(output) < 20:
        instructions, env, index = eval(instructions, env, index)
    print integer
    output = []
    integer += 1
    index = 0
    env = {"a": integer}
