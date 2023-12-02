from functools import reduce
import re

def findcolour(color: re.Pattern, s: str):
    m = re.search(color, s)
    if m is None:
        return -1
    return int(m.group(1))

with open("input.txt") as f:
    data = f.readlines()

# Split input into dict with id:data
matches = map(lambda x: re.match("Game ([0-9]+): (.+)$", x), data)
games = {int(m.group(1)): m.group(2).split("; ") for m in matches}

# Regex patterns
red = re.compile("([0-9]+) red")
green = re.compile("([0-9]+) green")
blue = re.compile("([0-9]+) blue")

# Get maximum number of each colour
games2 = {k: {"r": max(map(lambda x: findcolour(red, x), v)),
              "g": max(map(lambda x: findcolour(green, x), v)),
              "b": max(map(lambda x: findcolour(blue, x), v))}
          for k,v in games.items()}

# Sum ids if below thresholds
result_part1 = reduce(lambda acc, x: acc + x[0] if x[1]["r"] <= 12 and x[1]["g"] <= 13 and x[1]["b"] <= 14 else acc, 
                      games2.items(), 0)
print("Part 1:", result_part1)

# Sum products of each maximum
result_part2 = reduce(lambda acc, x: acc + x["r"] * x["g"] * x["b"], games2.values(), 0)
print("Part 2:", result_part2)
