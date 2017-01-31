import random
import pdb
compressed_file = open("day9.txt", "r")
compressed_data = compressed_file.read()

def find_blocks(compressed):
    index = 0
    blocks = []

    if compressed[0] != "(":
        blocks.append("")
    while index < len(compressed):
        if compressed[index] != "(":
            blocks[-1] += compressed[index]
            index += 1
        else:
            decoded, skip, repeat = "", "", ""
            index += 1
            while compressed[index] != "x":
                skip += compressed[index]
                index += 1
            # compressed[index] == "x", so let's skip that one
            index += 1
            while compressed[index] != ")":
                repeat += compressed[index]
                index += 1
            # compressed[index] == ")", so let's skip that one
            index += 1
            num_skip = int(skip)
            blocks.append("(" + skip + "x" + repeat + ")")
            while num_skip > 0:
                blocks[-1] += compressed[index]
                num_skip -= 1
                index += 1
            if index != len(compressed) and compressed[index] != "(":
                blocks.append("")

    return blocks

## ^^^^^ TESTED AND WORKING ^^^^^ ##

class Node():
    def __init__(self):
        self.repeats = 1
        self.value = 0
        self.children = []

    def add_child(self, block):
        child, index = Node(), 0
        if block[index] == "(":
            index += 1
            skip, repeats = "", ""
            while block[index] != "x":
                skip += block[index]
                index += 1
            # block[index] == "x", so let's skip that one
            index += 1
            while block[index] != ")":
                repeats += block[index]
                index += 1
            # block[index] == ")", so let's skip that one
            index += 1
            child.repeats = int(repeats)

        # THERE'S THE PROBLEM
            child.decompress(block[index:])
            self.children.append(child)
        # Even though the child has already been added, we're adding it again,
        # this time with the parentheses. There's no way to catch this without
        # including the parens somehow in the debug info, which I don't do. So,
        # if the first block triggers, I think I just set the repeats and then I
        # do the decompression, otherwise I just 
        else:
            self.children.append(child)
        
        # self.children.append(child.decompress(block[index:]))

    def update_value(self):
        value = 0
        for child in self.children:
            value += (child.repeats * child.value)

        self.value = value

    def decompress(self, compressed):
        # go through compressed and get all the sub-blocks
        # pdb.set_trace() # if you're in the debugger, hit n!!!
        blocks = find_blocks(compressed)
        if len(blocks) == 1 and compressed.find("(") == -1:
            self.value = len(compressed)
        else:
            print blocks
            for block in blocks:
                print block
                self.add_child(block)
            self.update_value()

test0 = "(9x2)(4x2)zach"
test1 = "(6x2)(1x3)saqa(2x5)hagood"
test2 = "too(15x2)(2x4)ha(3x2)fun"

root0, root1, root2, root3 = Node(), Node(), Node(), Node()
root0.decompress(test0)
print "root0 value =", root0.value
root1.decompress(test1)
print "root1 value =", root1.value
root2.decompress(test2)
print "root2 value =", root2.value
root3.decompress(compressed_data)
print "root3 value =", root3.value
# root is getting all the children, even though it should go into a lower level.

# root = decompress(1, compressed_data)
# print get_length(root)
