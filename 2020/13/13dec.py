import math

def part1(lines):
    timestamp = int(lines[0])
    buses = [int(bus) for bus in lines[1].split(",") if bus != "x"]
    wait = 0
    while True:
        for bus in buses:
            if (timestamp + wait) % bus == 0:
                print(bus * wait)
                return
        wait += 1

def part2(lines):
    allBuses = [int(bus) if bus != "x" else bus for bus in lines[1].split(",")]
    buses = [bus for bus in allBuses if bus != "x"]
    offsets = [allBuses.index(bus) for bus in buses]

    print(buses)
    print(offsets)

    maxIndex = buses.index(max(buses))
    timestamp = max(buses) - offsets[maxIndex]
    incr = max(buses)

    # iteration = 0
    while True:
        for i, bus in enumerate(buses):
            if (timestamp + offsets[i]) % bus != 0:
                break
        else:
            print(timestamp)
            return

        timestamp += incr
        # iteration += 1
        # if iteration % 1000000 == 0: # 10^6, ~1.4 sec
        #     print(iteration)

def part2v2(lines):
    allBuses = [int(bus) if bus != "x" else bus for bus in lines[1].split(",")]
    buses = [bus for bus in allBuses if bus != "x"]
    offsets = [allBuses.index(bus) for bus in buses]

    # kinesiska restsatsen
    N = math.prod(buses)
    x = 0
    for i, ni in enumerate(buses):
        a = -offsets[i] % ni
        x += int(a * findb(N, ni) * N/ni)
        # print("a:", a, " n:", ni, "offset:", offsets[i])

    # for i, bus in enumerate(buses):
    #     if x + offsets[i] % bus != 0:
    #         print(bus)
    print(x)


def findb(N, n):
    # b = 0
    # while (b*(N/n)) % n != 1:
    #     b += 1
    # print("b:", b)
    # return b
    return 1


with open("13dec-ex.txt", "r") as f:
    lines = f.readlines()

part2v2(lines)