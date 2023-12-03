def part1(lines):
    NS = 0 # North is positive
    EW = 0 # East is positive
    rotation = 3 # East

    for instruction in lines:
        op = instruction[0]
        value = int(instruction[1:])
        if   op == "N": NS += value
        elif op == "E": EW += value
        elif op == "S": NS -= value
        elif op == "W": EW -= value
        elif op == "R": rotation = (rotation - value/90) % 4
        elif op == "L": rotation = (rotation + value/90) % 4
        elif op == "F":
            if rotation % 2 == 0:
                r = -(rotation-1)
                NS += r * value # r determines positive or negative
            else:
                r = rotation-2
                EW += r * value

    distance = abs(NS) + abs(EW)
    print(distance)

def part2(lines):
    waypointNS = 1
    waypointEW = 10
    shipNS = 0
    shipEW = 0

    for instruction in lines:
        op = instruction[0]
        value = int(instruction[1:])

        if   op == "N": waypointNS += value
        elif op == "S": waypointNS -= value
        elif op == "E": waypointEW += value
        elif op == "W": waypointEW -= value
        elif op == "F":
            shipNS += value * waypointNS
            shipEW += value * waypointEW
        elif op == "R":
            while value > 0:
                waypointEW, waypointNS = waypointNS, -waypointEW
                value -= 90
        elif op == "L":
            while value > 0:
                waypointEW, waypointNS = -waypointNS, waypointEW
                value -= 90

    distance = abs(shipEW) + abs(shipNS)
    print(distance)


with open("12dec.txt", "r") as f:
    lines = f.readlines()
part2(lines)