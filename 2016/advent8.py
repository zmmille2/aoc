import numpy

screen = numpy.zeros((6, 50))

# create a rectangle: form is nxm
# ###
# ###
# above is 3x2
def rect(args):
    x, y = args[0].split("x")
    for width in xrange(int(x)):
        for height in xrange(int(y)):
            screen[height][width] = 1

def rotate(args):
    if args[0] == "column":
        column = int(args[1].split("=")[-1])
        rotations = int(args[3])
        screen[:,column] = numpy.roll(screen[:,column], rotations)
    else:
        row = int(args[1].split("=")[-1])
        rotations = int(args[3])
        screen[row] = numpy.roll(screen[row], rotations)
        pass

def execute(command):
    function = command.split(" ")[0]
    args = command.split(" ")[1:]

    if function == "rect":
        rect(args)
    elif function == "rotate":
        rotate(args)

commandsheet = open("day8.txt", "r")
commands = commandsheet.read()

testcommands = "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1"

for command in commands.split("\n"):
    execute(command)

lit_pixels = 0
for x in numpy.nditer(screen):
    lit_pixels += x

print screen
print lit_pixels
