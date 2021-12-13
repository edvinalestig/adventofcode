with open("3dec.txt", "r") as f:
    rows = f.readlines()

def findTrees(xStep, yStep):
    x, y, trees = 0, 0, 0
    for row in rows:
        if (x % xStep != 0):
            x += 1
            continue
        if row[y % (len(row)-1)] == "#":
            trees += 1
        y += yStep
        x += 1
    return trees

n1 = findTrees(1, 1)
n2 = findTrees(1, 3)
n3 = findTrees(1, 5)
n4 = findTrees(1, 7)
n5 = findTrees(2, 1)
print(n1, n2, n3, n4, n5)
print(n1*n2*n3*n4*n5)