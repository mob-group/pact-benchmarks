#!/usr/bin/env python

import random
import sys

def chunks(l,n):
    return [l[x: x+n] for x in xrange(0, len(l), n)]

def loc():
    return random.uniform(-1,1)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("No")
        sys.exit(1)
    n = int(sys.argv[1])
    print("natoms {}".format(n))
    types = chunks(['1'] * n, 10)
    print("typat")
    for tys in types:
        print("    {}".format(" ".join(tys)))
    print("xcart")
    for i in range(n):
        print("    {} {} {}".format(loc(), loc(), loc()))
