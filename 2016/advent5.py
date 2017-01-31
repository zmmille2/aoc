import hashlib
import sys

def hasBlanks(buckets):
    for item in buckets:
        if item == "":
            return True
    return False

door = "abbhdwsy"
hex_hash = ""
answer = ["" for x in xrange(8)]
index = 0
answer_index = 0

while hasBlanks(answer):
    m = hashlib.md5()
    m.update(door + str(index))
    hex_hash = m.hexdigest()
    if hex_hash.startswith("00000"):
        if hex_hash[5].isdigit():
            answer_index = int(hex_hash[5])
            print hex_hash
            if(answer_index < 8 and answer[answer_index] == ""):
                answer[answer_index] = hex_hash[6]
            print answer
    index += 1

print "".join(answer)
