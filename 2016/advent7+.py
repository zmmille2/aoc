def match(abas, babs):
    for aba in abas:
        print aba
        for bab in babs:
            print bab
            if aba[0] == bab[1] and bab[0] == aba[1]:
                return True

def ssl(segments):
    abas = []

    for segment in segments:
        for index in xrange(len(segment) - 2):
            if segment[index] == segment[index + 2]:
                abas.append(segment[index:index + 3])
    return abas

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

    return ssl(segments)

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

    return ssl(segments)

ips = open("day7.txt", "r")
support_ssl = 0

for line in ips.read().split("\n"):
    abas = brackets(line)
    babs = non_brackets(line)
    if match(abas, babs):
        support_ssl += 1

print support_ssl
