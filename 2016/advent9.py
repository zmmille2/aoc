import pdb
compressed_file = open("day9.txt", "r")
compressed = compressed_file.read()
decompressed = ""

def read_until_open_paren(string, start):
    read = ""
    index = start

    while index < len(string) and string[index] != "(":
        read += string[index]
        index += 1

    return read, index

def repeat_subsection(decompressed, compressed, index):
    x, y = "", ""
    index += 1
    while compressed[index] != "x":
        x += compressed[index]
        index += 1
    # compressed[index] == "x", so let's skip that one
    index += 1
    while compressed[index] != ")":
        y += compressed[index]
        index += 1
    # compressed[index] == ")", so let's skip that one, too
    index += 1

    repeat = compressed[index : index + int(x)]

    i = 0
    while i < int(y):
        decompressed += repeat
        i += 1
    index += int(x)

    return decompressed, index

def decompress(decompressed, compressed):
    result = ""
    index = 0

    while index < len(compressed):
        substring, index = read_until_open_paren(compressed, index)
        decompressed += substring
        if index < len(compressed):
            decompressed, index = repeat_subsection(decompressed, compressed, index)

    return decompressed

decompressed = decompress(decompressed, compressed)
while decompressed.find("(") != -1:
    compressed = decompressed
    decompressed = ""
    decompressed = decompress(decompressed, compressed)

print len(decompressed)
