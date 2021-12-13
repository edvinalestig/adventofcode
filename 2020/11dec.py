def occupiedAdjacentsPart1(matrix, row, col):
    count = 0
    for i in range(row-1, row+2):
        for j in range(col-1, col+2):
            if i == row and j == col: continue
            if i < 0 or j < 0: continue

            try:
                if matrix[i][j] == '#':
                    count += 1
            except IndexError:
                pass
    return count

def occupiedAdjacentsPart2(matrix, row, col):
    count = 0
    check = {
        "N":  True,
        "NE": True,
        "E":  True,
        "SE": True,
        "S":  True,
        "SW": True,
        "W":  True,
        "NW": True
    }

    i = 1
    while any(list(check.values())):
        w = len(matrix[0])
        h = len(matrix)

        if row-i < 0:                check["N"]  = False
        if row-i < 0 or col+i >= w:  check["NE"] = False
        if col+i >= w:               check["E"]  = False
        if row+i >= h or col+i >= w: check["SE"] = False
        if row+i >= h:               check["S"]  = False
        if row+i >= h or col-i < 0:  check["SW"] = False
        if col-i < 0:                check["W"]  = False
        if row-i < 0 or col-i < 0:   check["NW"] = False

        arr = occupiedAdjacentsPart2_2(matrix, row, col, i, check)
        for tup in arr:
            check[tup[0]] = False
            if tup[1] == "#":
                count += 1

        i += 1

    return count

def occupiedAdjacentsPart2_2(matrix, row, col, i, check):
    arr = []
    
    if check["NW"] and matrix[row-i][col-i] != ".":
        arr.append(("NW", matrix[row-i][col-i]))
    if check["N"] and matrix[row-i][col] != ".":
        arr.append(("N", matrix[row-i][col]))
    if check["NE"] and matrix[row-i][col+i] != ".":
        arr.append(("NE", matrix[row-i][col+i]))
    if check["E"] and matrix[row][col+i] != ".":
        arr.append(("E", matrix[row][col+i]))
    if check["SE"] and matrix[row+i][col+i] != ".":
        arr.append(("SE", matrix[row+i][col+i]))
    if check["S"] and matrix[row+i][col] != ".":
        arr.append(("S", matrix[row+i][col]))
    if check["SW"] and matrix[row+i][col-i] != ".":
        arr.append(("SW", matrix[row+i][col-i]))
    if check["W"] and matrix[row][col-i] != ".":
        arr.append(("W", matrix[row][col-i]))

        return arr


def applyRules(matrix):
    newMatrix = []
    for i, row in enumerate(matrix):
        newRow = []
        for j, cell in enumerate(row):

            if cell == "L" and occupiedAdjacentsPart2(matrix, i, j) == 0:
                newRow.append("#")
                continue

            if cell == "#" and occupiedAdjacentsPart2(matrix, i, j) >= 5:
                newRow.append("L")
                continue
            newRow.append(cell)

        newMatrix.append(newRow)

    return newMatrix

def countOccupied(matrix):
    occupied = 0
    for row in matrix:
        for cell in row:
            if cell == "#":
                occupied += 1
    return occupied

def part1(matrix):
    newMatrix = applyRules(matrix)
    while newMatrix != matrix:
        newMatrix, matrix = applyRules(newMatrix), newMatrix
        # print("\n".join(["".join(row) for row in matrix]), "\n")

    return countOccupied(matrix)

# -----------
with open("11dec.txt", "r") as f:
    matrix = [x.strip('\n') for x in f.readlines()]


print(part1(matrix))