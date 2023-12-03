from functools import reduce
import re
from numpy import prod

def findgear(arr: list[str], i: int, j: int) -> tuple[int, tuple[tuple[int, int], int] | None]:
    # Find number in string at coordinate (i,j)
    if not (m := re.match("([0-9]+)", arr[i][j:])):
        return (1, None)

    l: int = len(m.group(1))
    
    for i2 in range(i-1, i+2):
        try:
            # Check if * is near
            subarray = arr[i2][max(0,j-1) : j+l+1]
            if m2 := re.search("\*", subarray):
                # Return jump length, (* coord, value)
                return (l, (
                    (i2, max(0, j-1) + m2.start()), 
                    int(m.group(1))
                ))
        except IndexError:
            pass
    # No * found
    return (l, None)

with open("input.txt") as f:
    data = f.readlines()

gear_ratios = {}
for i,s in enumerate(data):
    j = 0
    while j < len(s):
        jump, d = findgear(data, i, j)
        j += jump
        if d is None: continue

        try:
            gear_ratios[d[0]].append(d[1])
        except KeyError:
            gear_ratios[d[0]] = [d[1]]
# Sum all gear ratios
result = reduce(lambda acc, x: acc + prod(x) if len(x) == 2 else acc, gear_ratios.values(), 0)

print(result)