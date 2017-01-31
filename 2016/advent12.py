instructions = open("day12.txt", "r").read().strip().split("\n")

index = 0
env = {'a':0, 'b':0, 'c':1, 'd':0}

def eval(instruction, environment, i):
    command = instruction[0]
    arg1 = instruction[1]
    if len(instruction) == 3:
        arg2 = instruction[2]

    if command == "cpy":
        if arg1.isdigit():
            environment[arg2] = int(arg1)
        else:
            environment[arg2] = environment[arg1]
        return environment, i + 1
    elif command == "inc":
        environment[arg1] += 1
        return environment, i + 1
    elif command == "dec":
        environment[arg1] -= 1
        return environment, i + 1
    elif command == "jnz":
        if arg1.isdigit() and int(arg1) != 0:
            return environment, i + int(arg2)
        elif environment[arg1] != 0:
            return environment, i + int(arg2)
        return environment, i + 1
    else:
        pass

while index < len(instructions):
    env, index = eval(instructions[index].split(" "), env, index)

for letter in ['a', 'b', 'c', 'd']:
    print letter, env[letter]
