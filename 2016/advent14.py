import md5

salt = "zpqevtbw"
test_salt = "abc"
keys = []
index = 0

def has_quints(key, repeat):
    return key.find(repeat * 5) != -1

def next_1000_has_quints(salt, index, repeat):
    i = 1
    while i < 1001:
        key = md5.new(salt + str(index + i)).hexdigest()
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

while len(keys) < 64:
    key = md5.new(salt + str(index)).hexdigest()
    if is_valid(salt, index, key):
        print key
        keys.append(index)
    index += 1

print keys[-1]
