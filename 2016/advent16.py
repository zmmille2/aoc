initial = "00111101111101000"
disk_size = 35651584

def generate_fill(start, size):
    data = start
    while len(data) < size:
        reverse = data.replace("0", "x").replace("1", "0").replace("x", "1")
        data += ("0" + reverse[::-1])

    return data

def generate_checksum(something, size):
    if len(something) > size:
        something = something[:size]
    index, checksum = 0, ""
    while index < len(something) - 1:
        if something[index] == something[index + 1]:
            checksum += "1"
        elif something[index] != something[index + 1]:
            checksum += "0"
        index += 2

    if len(checksum) % 2 == 0:
        return generate_checksum(checksum, size)
    return checksum


fill = generate_fill(initial, disk_size)
print fill
checksum = generate_checksum(fill, disk_size)
print checksum, len(checksum)
