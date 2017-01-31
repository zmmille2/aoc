import md5
from collections import defaultdict

# 13663 too low
salt = "zpqevtbw"
test_salt = "abc"
keys = []
index = 0
table = defaultdict(str)

def has_quints(key, repeat):
    return key.find(repeat * 5) != -1

def next_1000_has_quints(salt, index, repeat):
    i = 1
    while i < 1001:
        key = asinine_hash(salt + str(index + i))
        if has_quints(key, repeat):
            return True
        i += 1

    return False

def has_trips(key):
    first, second = "", ""
    for char in key:
        if first == "":
            first = char
        elif second == char:
            return True, char
        elif second == "" and first == char:
            second = char
        else:
            first, second = char, ""

    return False, ""

def is_valid(salt, index, key):
    trips, repeat = has_trips(key)
    if trips:
        return next_1000_has_quints(salt, index, repeat)
    return False

def asinine_hash(key):
    j = 0
    init = key
    if table[init] != "":
        return table[init]
    while j < 2017:
        key = md5.new(key).hexdigest()
        j += 1

    table[init] = key
    return key

while len(keys) < 64:
    key = asinine_hash(salt + str(index))
    if is_valid(salt, index, key):
        print key
        keys.append(index)
    index += 1

print keys
print len(keys)
