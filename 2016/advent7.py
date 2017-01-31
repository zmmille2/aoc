def is_reversible(segment):
    for index in xrange(len(segment) - 3):
        if segment[index] != segment[index + 1] and segment[index:index + 4] == ''.join(reversed(segment[index:index + 4])):
            return True
    return False

def has_reversible(segments):
    for segment in segments:
        if is_reversible(segment):
            return True
    return False

def non_brackets(line):
    in_brackets = False
    segments = [""]

    for char in list(line):
        if char == "[":
            in_brackets = True
        elif char == "]":
            segments.append("")
            in_brackets = False
        elif in_brackets:
            segments[-1] += char

    print "non_brackets:", segments

    return not has_reversible(segments)

def brackets(line):
    in_brackets = False
    segments = [""]

    for char in list(line):
        if char == "[":
            in_brackets = True
            segments.append("")
        elif char == "]":
            in_brackets = False 
        elif not in_brackets:
            segments[-1] += char

    print "brackets:", segments

    return has_reversible(segments)

ips = open("day7.txt", "r")
support_tls = 0

for line in ips.read().split("\n"):
    print "---"
    print line
    if brackets(line) and non_brackets(line):
        print True
        support_tls += 1

print support_tls
