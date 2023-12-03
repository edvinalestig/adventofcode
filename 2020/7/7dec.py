class Bag():
    def __init__(self, line):
        self.bag = line.split(" bags")[0]
        self.contents = []
        content = line.split("contain ")[1].split(" bags, ")
        content2  = []
        for c in content:
            content2 += c.split(" bag, ")

        for s in content2:
            num = s.split(" ")[0]
            if num != "no":
                colour = s.split(" ")[1] + " " + s.split(" ")[2]
                self.contents.append({
                    "colour": colour,
                    "num": int(num)
                })
        # print(self.bag, "->", self.contents)


class Node():
    def __init__(self, key, value):
        self.key = key
        self.value = value
        self.left = None
        self.right = None
    
    def add(self, key, value):
        if key < self.key:
            if self.left == None:
                self.left = Node(key, value)
            else:
                self.left.add(key, value)
        elif key > self.key:
            if self.right == None:
                self.right = Node(key, value)
            else:
                self.right.add(key, value)
        
    def find(self, key):
        if key == self.key:
            return self.value
        elif key < self.key:
            if self.left == None:
                return None
            else:
                return self.left.find(key)
        else:
            if self.right == None:
                return None
            else:
                return self.right.find(key)

    def size(self):
        if self.left == None and self.right == None:
            return 1
        elif self.left == None:
            return 1 + self.right.size()
        elif self.right == None:
            return 1 + self.left.size()
        else:
            return 1 + self.left.size() + self.right.size()

    def containsGold(self, bagColour):
        if bagColour == "shiny gold":
            return True
        elif bagColour == None:
            return False
        
        bagObj = self.find(bagColour)
        if bagObj == None:
            return False
        else:
            for bag in bagObj.contents:
                # print("recurse at", bagColour)
                if self.containsGold(bag["colour"]):
                    return True
            return False

def countBags(tree, bagColour):
    # if bagColour == None:
    #     return 0
    bagObj = tree.find(bagColour)
    # if bagObj == None:
    #     return 0

    count = 0
    for bag in bagObj.contents:
        n = bag["num"]
        recN = countBags(tree, bag["colour"])
        print(recN)
        count += (n * recN)

    return count + 1


def part1(lines):
    bagColours = [lines[0].split(" bags")[0]]
    bags = Node(lines[0].split(" bags")[0], Bag(lines[0]))
    first = lines[0]
    for line in lines:
        if line == first:
            continue
        b = Bag(line)
        bags.add(b.bag, b)
        col = line.split(" bags")[0]
        if col != "shiny gold":
            bagColours.append(col)
        
    count = 0
    for colour in bagColours:
        if bags.containsGold(colour):
            count += 1
    
    print(count)
    print("Size:", bags.size())


def part2(lines):
    bags = Node(lines[0].split(" bags")[0], Bag(lines[0]))
    first = lines[0]

    for line in lines:
        if line == first:
            continue
        b = Bag(line)
        bags.add(b.bag, b)

    count = countBags(bags, "shiny gold")
    print("Count:", count)



with open("7dec.txt", "r") as f:
    lines = f.readlines()

part2(lines)