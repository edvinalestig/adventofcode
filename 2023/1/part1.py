with open("input.txt") as f:
    inputs = f.readlines()

def firstdigit(s):
    for d in s:
        if d in "0123456789": 
            return d
        
    raise ValueError("No digit found")

def lastdigit(s):
    return firstdigit(reversed(s))

print(sum([int(f"{firstdigit(s)}{lastdigit(s)}") for s in inputs]))
