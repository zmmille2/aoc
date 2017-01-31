from collections import defaultdict
import operator

messages = open("day6.txt", 'r')
frequencies = defaultdict(lambda : defaultdict(int))

test = ["eedadn"
,"drvtee"
,"eandsr"
,"raavrd"
,"atevrs"
,"tsrnev"
,"sdttsa"
,"rasrtv"
,"nssdts"
,"ntnada"
,"svetve"
,"tesnvt"
,"vntsnd"
,"vrdear"
,"dvrsen"
,"enarar"]

lines = messages.read().split("\n")
for line in lines:
    array = list(line)
    for index in xrange(len(array)):
        frequencies[index][array[index]] += 1

for index in frequencies.keys():
    for letter in frequencies[index].keys():
        if frequencies[index][letter] == 20:
            print letter,

messages.close()
