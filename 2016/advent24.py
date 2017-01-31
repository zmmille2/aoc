import numpy
import Queue
from collections import defaultdict

maze = open("day24.txt", "r").read().strip().split("\n")

checkpoints = {0:(3,5), 1:(3,53), 2:(21,1), 3:(3,181), 4:(39,11), 5:(33,149), 6:(43,141), 7:(21,169)}
walls = numpy.zeros((len(maze), len(maze[0])), dtype=int).tolist()
reset = numpy.zeros((len(maze), len(maze[0])), dtype=int).tolist()
queue = Queue.Queue()

def BFS(step, visited):
    [(x, y), goal, distance] = step
    for new_checkpoint in [(x+1,y), (x,y+1), (x-1,y), (x,y-1)]:
        (new_x, new_y) = new_checkpoint
        target = visited[new_x][new_y]
        if target == 0:
            queue.put([new_checkpoint, goal, distance + 1])

def distance(checkpoint, goal):
    queue.put([checkpoint, goal, 0])
    while not queue.empty():
        step = queue.get(1)
        (x, y) = step[0]
        if walls[x][y] == 0:
            walls[x][y] = step[2]
            if step[0] == step[1]:
                return step
            BFS(step, walls)

for x in xrange(len(maze)):
    for y in xrange(len(maze[x])):
        if maze[x][y] == "#":
            walls[x][y] = -1
            reset[x][y] = -1

grid = defaultdict(lambda : defaultdict(int))

for checknum in checkpoints.keys():
    checkpoint = checkpoints.pop(checknum)
    for goalnum in checkpoints.keys():
        goal = checkpoints[goalnum]
        grid[checknum][goalnum] = distance(checkpoint, goal)[2]
        queue = Queue.Queue()
        walls = numpy.copy(reset).tolist()

for x in grid.keys():
    print x
    for y in grid[x].keys():
        print "\t" + str(y) + " " + str(grid[x][y])
