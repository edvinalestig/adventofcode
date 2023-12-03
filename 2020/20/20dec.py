from random import randint

class Tile():
    def __init__(self, text):
        lines = text.split("\n")
        self.id     = int(lines[0].split(" ")[1].split(":")[0])
        self.left   = "".join([s[0] for s in lines if s[0] != "T"])
        self.right  = "".join([s[-1] for s in lines if s[-1] != ":"])
        self.top    = lines[1]
        self.bottom = lines[-1]
        self.body = []
        for l in lines[2:-2]:
            self.body.append(l[1:-2])

        self.checked = []

        self.topNb = None
        self.bottomNb = None
        self.leftNb = None
        self.rightNb = None
        # print(len(self.body), len(self.body[0]))


    def __str__(self):
        return f"Tile {self.id}"

    def __repr__(self):
        return self.__str__()

    def rotate(self):
        self.left, self.top, self.right, self.bottom = self.bottom, self.left[::-1], self.top, self.right[::-1]
        self.body = ["".join([s[i] for s in self.body]) for i in range(len(self.body))] # hopefully works

    def flipV(self):
        self.top, self.bottom = self.bottom, self.top
        self.left = self.left[::-1]
        self.right = self.right[::-1]
        self.body = self.body[::-1]

    def flipH(self):
        self.left, self.right = self.right, self.left
        self.top = self.top[::-1]
        self.bottom = self.bottom[::-1]
        for r in range(len(self.body)):
            self.body[r] = self.body[r][::-1]

    def __contains__(self, item):
        if item == self.left or item == self.right or item == self.top or item == self.bottom:
            return True
        i = item[::-1]
        return i == self.left or i == self.right or i == self.bottom or i == self.top

    def pos(self, item):
        if item == self.left:   return "left"
        if item == self.right:  return "right"
        if item == self.top:    return "top"
        if item == self.bottom: return "bottom"
        if item[::-1] == self.left:   return "rev left"
        if item[::-1] == self.top:    return "rev top"
        if item[::-1] == self.right:  return "rev right"
        if item[::-1] == self.bottom: return "rev bottom"
        return ""



def part1(text):
    tiles = [Tile(t) for t in text.split("\n\n") if t != ""]

    for i in range(len(tiles)):
        for j in range(len(tiles)):
            if i == j: continue
            skip = False

            t1 = tiles[i]
            t2 = tiles[j]

            if t1.left in t2:
                t1.leftNb = t2.id
            elif t1.top in t2:
                t1.topNb = t2.id
            elif t1.right in t2:
                t1.rightNb = t2.id
            elif t1.bottom in t2:
                t1.bottomNb = t2.id


    corners = []
    for t in tiles:
        nb = 0
        if t.leftNb:
            nb += 1
        if t.rightNb:
            nb += 1
        if t.bottomNb:
            nb += 1
        if t.topNb:
            nb += 1
        # print(nb)

        if nb <= 2:
            corners.append(t)

    print(corners)
    p = 1
    for c in corners:
        p *= c.id
    print(p)

def assembleImage(tiles):
    pass

def part2(text):
    tiles = [Tile(t) for t in text.split("\n\n") if t != ""]

    for i in range(len(tiles)):
        for j in range(len(tiles)):
            if i == j: continue

            t1 = tiles[i]
            t2 = tiles[j]
            if t1.id in t2.checked: continue

            if t1.left in t2:
                while t2.pos(t1.left) != "right":
                    r = randint(0, 100)
                    if r > 75:
                        t1.flipH()
                    elif r > 50:
                        t1.flipV()
                    t1.rotate()
                t1.leftNb = t2.id
                t2.rightNb = t1.id
            elif t1.top in t2:
                while t2.pos(t1.top) != "bottom":
                    r = randint(0, 100)
                    if r > 75:
                        t1.flipH()
                    elif r > 50:
                        t1.flipV()
                    t1.rotate()
                t1.topNb = t2.id
                t2.bottomNb = t1.id
            elif t1.right in t2:
                while t2.pos(t1.right) != "left":
                    r = randint(0, 100)
                    if r > 75:
                        t1.flipH()
                    elif r > 50:
                        t1.flipV()
                    t1.rotate()
                t1.rightNb = t2.id
                t2.leftNb = t1.id
            elif t1.bottom in t2:
                while t2.pos(t1.bottom) != "top":
                    r = randint(0, 100)
                    if r > 75:
                        t1.flipH()
                    elif r > 50:
                        t1.flipV()
                    t1.rotate()
                t1.bottomNb = t2.id
                t2.topNb = t1.id
            # print("exited loop")
            t1.checked = t2.id
            t2.checked = t1.id

    for t in tiles:
        print(t)

with open("20dec.txt", "r") as f:
    text = f.read()

part2(text)