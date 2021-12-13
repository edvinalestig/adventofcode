class Rule():
    def __init__(self, ruleName, range1Low, range1High, range2Low, range2High):
        self.name = ruleName
        self.range1 = (int(range1Low), int(range1High))
        self.range2 = (int(range2Low), int(range2High))
        self.position = None

    def inRange(self, i):
        if (i >= self.range1[0]) and (i <= self.range1[1]):
            return True
        if (i >= self.range2[0]) and (i <= self.range2[1]):
            return True
        return False

# n: number of tickets
# m: number of rules

# O(n*m^2)
def part1(rules, nearbyTickets):
    sumOfInvalid = 0
    for ticket in nearbyTickets:
        for t in ticket.split(","):
            for rule in rules:
                if rule.inRange(int(t)):
                    break
            else:
                sumOfInvalid += int(t)

    print(sumOfInvalid)

# O(n*m^2 + n^2*m + ?) tror jag
def part2(rules, myTicket, nearbyTickets):
    validTickets = []
    nearbyTickets = [[int(t) for t in ticket.split(",")] for ticket in nearbyTickets]
    myTicket = [[int(t) for t in myTicket.split(",")]]

    # O(n*m^2)
    for ticket in nearbyTickets:
        valid = True
        for field in ticket:
            for rule in rules:
                if rule.inRange(field):
                    break
            else:
                valid = False
                break
        if valid:
            validTickets.append(ticket)

    # O(n^2*m)
    for rule in rules:
        i = 0
        valid = []
        while i < len(validTickets[0]):
            for ticket in (validTickets + myTicket):
                if not rule.inRange(ticket[i]):
                    break
            else:
                valid.append(i)

            i += 1
        rule.position = valid

    # O(?)
    while True:
        for rule in rules:
            if len(rule.position) == 1:
                for r in rules:
                    if r.name == rule.name: continue
                    try: r.position.remove(rule.position[0])
                    except ValueError: pass


        for rule in rules:
            if len(rule.position) != 1:
                break
        else:
            break

    p = 1
    for rule in rules:
        rule.position = rule.position[0]
        print(rule.name, rule.position)
        if "departure" in rule.name:
            p *= myTicket[0][rule.position]

    print(p)
            





with open("16dec.txt", "r") as f:
    text = f.read()
rules, myTicket, nearbyTickets = text.split("\n\n")

myTicket = myTicket.split("\n")[1]
nearbyTickets = nearbyTickets.split(":\n")[1].split("\n") # List of tickets

ruleList = []
for rule in rules.split("\n"):
    name, ranges = rule.split(": ")
    range1, range2 = ranges.split(" or ")
    r1L, r1H = range1.split("-")
    r2L, r2H = range2.split("-")
    ruleList.append(Rule(name, r1L, r1H, r2L, r2H))

part2(ruleList, myTicket, nearbyTickets)
