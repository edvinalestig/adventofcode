def countActiveNeighbours(zyx, x, y, z):
    count = 0
    for X in range(x-1, x+2):
        for Y in range(y-1, y+2):
            for Z in range(z-1, z+2):
                if X == x and Y == y and Z == z:
                    continue
                try: 
                    if zyx[Z][Y][X] == "#": count += 1
                except IndexError: 
                    pass
    return count

def expandDimension(zyx):
    ys = len(zyx[0])
    xs = len(zyx[0][0])
    # print("Expanding dimensions:", xs, ys, len(zyx))

    for z in zyx:
        for y in z:
            y.insert(0, ".")
            y.append(".")
        z.insert(0, ["." for i in range(ys+2)])
        z.append(["." for i in range(ys+2)])

    zyx.insert(0, [["." for i in range(xs+2)] for j in range(ys+2)])
    zyx.append([["." for i in range(xs+2)] for j in range(ys+2)])
    return zyx

def applyRules(zyx):
    zyx = expandDimension(zyx)
    newZyx = []
    for z, plane in enumerate(zyx):
        newPlane = []
        for y, line in enumerate(plane):
            newLine = []
            for x, cube in enumerate(line):
                count = countActiveNeighbours(zyx, x, y, z)
                if cube == "#":
                    if count == 2 or count == 3:
                        newLine.append("#")
                    else:
                        newLine.append(".")
                else:
                    if count == 3:
                        newLine.append("#")
                    else:
                        newLine.append(".")

            newPlane.append(newLine)
        newZyx.append(newPlane)

    return newZyx

def countAllActive(zyx):
    count = 0
    for z in zyx:
        for y in z:
            for x in y:
                if x == "#":
                    count += 1
    return count

def checkLengths(zyx):
    ys = len(zyx[0])
    xs = len(zyx[0][0])
    for z in zyx:
        for y in z:
            if len(y) != xs:
                return False
        if len(z) != ys:
            return False
    return True

def part1(zyx):
    for i in range(6):
        zyx = applyRules(zyx)
    # print(checkLengths(zyx))
    print(countAllActive(zyx))

# ------------

def countActiveNeighbours2(zyx, x, y, z, w):
    count = 0
    for X in range(x-1, x+2):
        for Y in range(y-1, y+2):
            for Z in range(z-1, z+2):
                for W in range(w-1, w+2):
                    if X == x and Y == y and Z == z and W == w:
                        continue

                    try: 
                        if zyx[W][Z][Y][X] == "#": count += 1
                    except IndexError: 
                        pass

    return count

def expandDimension2(wzyx):
    zs = len(wzyx[0])
    ys = len(wzyx[0][0])
    xs = len(wzyx[0][0][0])
    print("Expanding dimensions:", len(wzyx), zs, ys, xs)

    for w in wzyx:
        for z in w:
            for y in z:
                y.insert(0, ".")
                y.append(".")
            l = ["." for i in range(xs+2)]
            z.insert(0, l)
            z.append(l)
        ll = [l for j in range(ys+2)]
        w.insert(0, ll)
        w.append(ll)
    lll = [ll for k in range(zs+2)]
    wzyx.insert(0, lll)
    wzyx.append(lll)
    
    return wzyx

def applyRules2(wzyx):
    wzyx = expandDimension2(wzyx)
    newWzyx = []
    for w, space in enumerate(wzyx):
        newSpace = []
        for z, plane in enumerate(space):
            newPlane = []
            for y, line in enumerate(plane):
                newLine = []
                for x, cube in enumerate(line):
                    count = countActiveNeighbours2(wzyx, x, y, z, w)
                    if cube == "#":
                        if count == 2 or count == 3:
                            newLine.append("#")
                        else:
                            newLine.append(".")
                    else:
                        if count == 3:
                            newLine.append("#")
                        else:
                            newLine.append(".")

                newPlane.append(newLine)
            newSpace.append(newPlane)
        newWzyx.append(newSpace)

    return newWzyx

def countAllActive2(wzyx):
    count = 0
    for space in wzyx:
        for plane in space:
            for line in plane:
                for cube in line:
                    if cube == "#":
                        count += 1
    return count

def checkLengths2(wzyx):
    zs = len(wzyx[0])
    ys = len(wzyx[0][0])
    xs = len(wzyx[0][0][0])
    for w in wzyx:
        for z in w:
            for y in z:
                if len(y) != xs:
                    return False
            if len(z) != ys:
                return False
        if len(w) != zs:
            return False
    return True

def part2(wzyx):
    wzyx = [wzyx]
    for i in range(6):
        wzyx = applyRules2(wzyx)
    # print(checkLengths2(wzyx))
    print(countAllActive2(wzyx))




with open("17dec.txt", "r") as f:
    zyx = [[[c for c in r if c != "\n"] for r in f.readlines()]]

# zyx[z][y][x]
part2(zyx)