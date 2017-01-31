from collections import defaultdict

# 4918 too low
# stolen from reddit :(
num_elves = 3001330

class Node:
    def __init__(self, value):
        self.value = value
        self.nxt = None
        self.prv = None

    def delete(self):
        self.prv.nxt = self.nxt
        self.nxt.prv = self.prv

def solve(n):
    l = map(Node, xrange(n))
    for i in xrange(n):
        l[i].nxt = l[(i+1)%n]
        l[i].prv = l[(i-1)%n]

    start = l[0]
    mid   = l[n/2]

    for i in xrange(n-1):
        mid.delete()
        mid = mid.nxt
        if (n-i)%2==1: mid = mid.nxt
        start = start.nxt

    return start.value + 1

print solve(num_elves)
