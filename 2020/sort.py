day = "10"

with open(f"{day}dec.txt", "r") as f:
    arr = [int(i) for i in f.readlines()]

arr.sort()
with open(f"{day}dec-sorted.txt", "w") as f:
    f.write("\n".join([str(i) for i in arr]))