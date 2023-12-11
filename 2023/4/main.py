import re

regex = re.compile("Card +([0-9]+): ([0-9 ]+) \| ([0-9 ]+)")

with open("input.txt") as f:
    data = f.readlines()

matches = map(lambda x: re.match(regex, x), data)
numbers = {int(m.group(1)): (set(filter(lambda x: x != "", m.group(2).split(" "))), 
            set(m.group(3).split(" "))) for m in matches}
intersections = {card: len(x[0].intersection(x[1])) for card,x in numbers.items()}

# Part 1
points = map(lambda x: 2**(x-1) if x else 0, intersections.values())
print(sum(points))

# Part 2
cards = {card_num: 1 for card_num in intersections.keys()}
for card,nums in intersections.items():
    for i in range(nums):
        cards[card+i+1] += cards[card]
print(sum(cards.values()))
