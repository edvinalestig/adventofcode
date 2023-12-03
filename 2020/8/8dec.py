SP = 0
REG_A = 0

def jmp(steps):
    global SP
    SP += int(steps)

def nop():
    global SP
    SP += 1

def acc(value):
    global SP
    global REG_A
    REG_A += int(value)
    SP += 1

def part1(lines):
    global SP
    global REG_A

    repeats = []
    while SP not in repeats:
        repeats.append(SP)
        line = lines[SP]
        if "nop" in line:
            nop()
        elif "jmp" in line:
            jmp(line.split("jmp ")[1])
        elif "acc" in line:
            acc(line.split("acc ")[1])

    print(REG_A)

def explore(SP, instructions):
    repeats = []
    while SP not in repeats:
        repeats.append(SP)
        op, value = instructions[SP].split(" ")
        value = int(value)

        if op == "jmp":
            SP += value - 1
        SP += 1
        if SP >= len(instructions):
            return True

    return False

def part2(instructions):
    global SP
    global REG_A
    changed = False

    while SP < len(instructions):
        op, val = instructions[SP].split(" ")
        val = int(val)

        if op == "acc":
            acc(val)

        elif op == "nop":
            if not changed and explore(SP + val, instructions):
                jmp(val)
                changed = True
                # print("nop -> jmp")
            else:
                nop()

        elif op == "jmp":
            if not changed and explore(SP + 1, instructions):
                nop()
                changed = True
                # print("jmp -> nop")
            else:
                jmp(val)
        # print(SP)


    print(REG_A)

with open("8dec.txt", "r") as f:
    lines = f.readlines()
    
part1(lines)
