def part1(lines):

    preambleNum = 25
    i = preambleNum

    notFound = []
    while i < len(lines):
        preambleList = lines[i-preambleNum : i]

        foundSum = False
        for j in preambleList:
            if lines[i] - j in preambleList:
                foundSum = True
                break
            

        if not foundSum:
            # print("No sum:", lines[i])
            notFound.append(lines[i])

        i += 1

    return notFound[0]

def part2(lines, num):
    index = lines.index(num)
    i = 0
    found = False
    rang = []
    while i < index and not found:
        rang = []
        sum = 0
        for k in lines[i:index]:
            rang.append(k)
            sum += k
            if sum == num:
                found = True
                break
            if sum > num:
                break
        i += 1

    # print(rang)

    return rang



with open("9dec.txt", "r") as f:
    lines = [int(i) for i in f.readlines()]

# print(part1(lines))
p2 = part2(lines, 1492208709)
print(sum(p2))
print(min(p2) + max(p2))