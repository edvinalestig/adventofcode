def clean(string):
    newStr = ""
    for c in string:
        if c not in newStr and c != '\n':
            newStr += c
    return newStr

def part1(lines):
    groups = []
    temp = ""
    for line in lines:
        if line == "\n":
            groups.append(temp)
            temp = ""
        else:
            temp += line
    if temp != "":
        groups.append(temp)

    chopped = [clean(group) for group in groups]

    count = 0
    for c in chopped:
        count += len(c)

    print(count)

def chop(arr):
    newStr = ""
    for c in "abcdefghijklmnopqrstuvwxyz":
        add = True
        for a in arr:
            if c not in a:
                add = False
                break
        if add:
            newStr += c
    return newStr


def part2(lines):
    groups = []
    temp = []
    for line in lines:
        if line != "\n":
            temp.append(line)
        else:
            groups.append(temp)
            temp = []
    if temp != []:
        groups.append(temp)

    chopped = [chop(group) for group in groups]
    # print(chopped)

    count = 0
    for c in chopped:
        count += len(c)

    print(count)


with open("6dec.txt", "r") as f:
    lines = f.readlines()
part2(lines)
