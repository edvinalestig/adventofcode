

with open("2dec.txt") as f:
    inputs = f.readlines()


def part1(inputs):
    count = 0
    for psw in inputs:
        mn, rest = psw.split("-")
        mx, c, passw = rest.split(" ")
        c = c[0]
        mn = int(mn)
        mx = int(mx)
        if passw.count(c) <= mx and passw.count(c) >= mn:
            count += 1

        # print("c:", c, ", mn:", mn, ", mx:", mx, ", pass:", passw)

    return count
    
def part2(inputs):
    count = 0
    for psw in inputs:
        mn, rest = psw.split("-")
        mx, c, passw = rest.split(" ")
        c = c[0]
        mn = int(mn)
        mx = int(mx)
        if passw[mn-1] == c and passw[mx-1] != c:
            count += 1
        elif passw[mn-1] != c and passw[mx-1] == c:
            count += 1

        # print("c:", c, ", mn:", mn, ", mx:", mx, ", pass:", passw)

    return count

print("Part1:", part1(inputs))
print("Part2:", part2(inputs))