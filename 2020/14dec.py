def part1(lines):
    mem = {}
    addOne  = 0b0 # use with |
    addZero = 0b0 # use with &

    for line in lines:
        if "mask" in line:
            line = line.split(" = ")[1]
            addOne  = int(line.replace('X', '0'), 2)
            addZero = int(line.replace('X', '1'), 2)
        elif "mem[" in line:
            address = int(line.split("mem[")[1].split("]")[0])
            value   = int(line.split(" = ")[1])

            # Apply bitmask
            value &= addZero
            value |= addOne
            mem[address] = value

    s = sum(mem.values())

    print("Sum:", s)


def getAddresses(mask, address, i=0):
    if i >= len(mask):
        return [address]
    elif mask[i] == "X":
        addr = bin(address).split("0b")[1]
        addr = "0" * (36-len(addr)) + addr
        addr0 = int(addr[:i] + "0" + addr[i+1:], 2)
        addr1 = int(addr[:i] + "1" + addr[i+1:], 2)
        return getAddresses(mask, addr0, i+1) + getAddresses(mask, addr1, i+1)
    elif mask[i] == "1":
        addr = bin(address).split("0b")[1]
        addr = "0" * (36-len(addr)) + addr
        addr = addr[:i] + "1" + addr[i+1:]
        address = int(addr, 2)
    return getAddresses(mask, address, i+1)

def part2(lines):
    mem = {}
    mask = ""
    for line in lines:
        if "mask" in line:
            mask = line.split(" = ")[1]
        else:
            address = int(line.split("mem[")[1].split("]")[0])
            value = int(line.split(" = ")[1])

            # print("Mask:", "\n" + mask)
            # print("Address:", bin(address))
            for addr in getAddresses(mask, address):
                mem[addr] = value
                # s = bin(addr).split("0b")[1]
                # print("0" * (36-len(s)) + s)
            # input()

    print(sum(mem.values()))

with open("14dec.txt", "r") as f:
    lines = f.readlines()

part2(lines)