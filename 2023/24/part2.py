import numpy as np

def makeHail(s: str):
    pos, vel         = s.split(" @ ")
    x, y, z          = pos.split(", ")
    xvel, yvel, zvel = vel.split(", ")
    return (int(x), int(y), int(z), int(xvel), int(yvel), int(zvel))

with open("input.txt") as f:
    data = f.readlines()

hails = list(map(makeHail, data))

left_m  = []
right_m = []

i = 0
while i < 8:
    x1, y1, z1, dx1, dy1, dz1 = hails[i]
    x2, y2, z2, dx2, dy2, dz2 = hails[i+1]
    left_m.append([dy2-dy1, y1-y2, dx1-dx2, x2-x1])
    right_m.append(y1*dx1 - x1*dy1 - y2*dx2 + x2*dy2)
    i += 2

a = np.array(left_m)
b = np.array(right_m)
X = np.linalg.solve(a, b) # Solve for x, dx, y, dy

left_m  = []
right_m = []
i = 0
while i < 2:
    x1, y1, z1, dx1, dy1, dz1 = hails[i]
    y, dy = X[2], X[3]
    left_m.append([dy-dy1, y1-y])
    right_m.append((z1*dy-z1*dy1) + (dz1*y1-dz1*y))
    i += 1

a = np.array(left_m)
b = np.array(right_m)
X2 = np.linalg.solve(a, b) # solve for z, dz

print(X[0], X[2], X2[0])
print(sum([X[0], X[2], X2[0]]))
