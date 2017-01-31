from collections import defaultdict
import Queue

instructions = open("day10.txt", "r").read().strip()

bots = defaultdict(list)
outputs = {}
instruction_queue = Queue.Queue()

def set_value(instruction):
    bots[int(instruction[-1])].append(int(instruction[1]))

#this always runs after give_low, let's be lazy lol
def give_high(give, high):
    print give, high
    if bots[give][0] > bots[give][1]:
        gift = bots[give][0]
    else:
        gift = bots[give][1]
    if high[0] == "output":
        outputs[int(high[1])] = gift
    else:
        bots[int(high[1])].append(gift)

def give_low(give, low):
    print give, low
    first = bots[give][0]
    second = bots[give][1]
    if first < second:
        if low[0] == "output":
            outputs[int(low[1])] = first
        else:
            bots[int(low[1])].append(first)
    else:
        if low[0] == "output":
            outputs[int(low[1])] = second
        else:
            bots[int(low[1])].append(second)

def really_make_trade(give, low, high):
    give_low(int(give), low)
    give_high(int(give), high)

def make_trade(instruction):
    print instruction
    really_make_trade(instruction[1], instruction[5:7], instruction[10:12])

def can_perform(instruction):
    if instruction[0] == "value":
        return True
    else:
        return len(bots[int(instruction[1])]) == 2
    
def perform(instruction):
    if instruction[0] == "value":
        set_value(instruction)
    else:
        make_trade(instruction)

for instruction in instructions.split("\n"):
    instruction_queue.put(instruction)

while instruction_queue.not_empty:
    try: instruction = instruction_queue.get(True, 2)
    except: break
    words = instruction.split(" ")
    if can_perform(words):
        perform(words)
    else:
        instruction_queue.put(instruction)

print "I'm done."
for output in outputs.keys():
    print output, outputs[output]
