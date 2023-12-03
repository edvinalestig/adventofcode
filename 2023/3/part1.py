import re

def findsymbols(arr: list[str], i: int, j: int) -> tuple[int, int]:
    # Find number at (i,j)
    if not (m := re.match("([0-9]+)", arr[i][j:])):
        return (1, 0)

    l: int = len(m.group(1))
    
    for i2 in range(i-1, i+2):
        try:
            # Find symbol near the number
            subarray = arr[i2][max(0,j-1) : j+l+1]
            if re.search("[^0-9\.\n]", subarray):
                return (l, int(m.group(1)))
        except IndexError:
            pass
    # No symbol found
    return (l, 0)

with open("input.txt") as f:
    data = f.readlines()

acc = 0
for i,s in enumerate(data):
    j = 0
    while j < len(s):
        # Go through all (i,j) in the input data
        jump, num = findsymbols(data, i, j)
        acc += num
        # Jump ahead if number was found to prevent counting twice
        j += jump

print(acc)