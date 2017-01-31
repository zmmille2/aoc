instructions = ['R4', 'R3', 'R5', 'L3', 'L5', 'R2', 'L2', 'R5', 'L2', 'R5', 'R5', 'R5', 'R1', 'R3', 'L2', 'L2', 'L1', 'R5', 'L3', 'R1', 'L2', 'R1', 'L3', 'L5', 'L1', 'R3', 'L4', 'R2', 'R4', 'L3', 'L1', 'R4', 'L4', 'R3', 'L5', 'L3', 'R188', 'R4', 'L1', 'R48', 'L5', 'R4', 'R71', 'R3', 'L2', 'R188', 'L3', 'R2', 'L3', 'R3', 'L5', 'L1', 'R1', 'L2', 'L4', 'L2', 'R5', 'L3', 'R3', 'R3', 'R4', 'L3', 'L4', 'R5', 'L4', 'L4', 'R3', 'R4', 'L4', 'R1', 'L3', 'L1', 'L1', 'R4', 'R1', 'L4', 'R1', 'L1', 'L3', 'R2', 'L2', 'R2', 'L1', 'R5', 'R3', 'R4', 'L5', 'R2', 'R5', 'L5', 'R1', 'R2', 'L1', 'L3', 'R3', 'R1', 'R3', 'L4', 'R4', 'L4', 'L1', 'R1', 'L2', 'L2', 'L4', 'R1', 'L3', 'R4', 'L2', 'R3', 'L1', 'L5', 'R4', 'R5', 'R2', 'R5', 'R1', 'R5', 'R1', 'R3', 'L3', 'L2', 'L2', 'L5', 'R2', 'L2', 'R5', 'R5', 'L2', 'R3', 'L5', 'R5', 'L2', 'R4', 'R2', 'L1', 'R3', 'L5', 'R3', 'R2', 'R5', 'L1', 'R3', 'L2', 'R2', 'R1']

# 0 = N, 3 = W
def update_direction(direction, instruction):
    direction = direction + 1 if instruction[0] == 'R' else direction - 1
    direction %= 4
    return direction

def update_position(position, direction, instruction):
    steps = int(instruction[1:])

    if(direction > 1):
        steps = -steps
        direction = direction - 2

    position[direction] += steps
    return position

curr_pos = [0, 0]
curr_dir = 0

for instruction in instructions:
    # turn the given direction, then go that many forward.
    curr_dir = update_direction(curr_dir, instruction)
    curr_pos = update_position(curr_pos, curr_dir, instruction)
print curr_pos[0] + curr_pos[1]
