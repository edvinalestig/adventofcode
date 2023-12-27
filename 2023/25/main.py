import numpy as np
import igraph as ig

def part1(data: list[str]) -> None:
    edges: list[tuple[str,str]] = []
    for d in data:
        node, cons = d.split(": ")
        edges.extend([(node, c) for c in cons.split(" ")])

    mincut: ig.Cut = ig.Graph.TupleList(edges).mincut()

    print(np.prod(list(map(len, mincut.partition))))

with open("input.txt") as f:
    data = f.read().strip().split("\n")

part1(data)
