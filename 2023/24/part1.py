from itertools import combinations
from functools import reduce
from numpy import sign

def make_2d_lines(line: str) -> tuple[int,int,int,int]:
    point, direction = line.split(" @ ")
    x,  y,  _ = map(int, point.split(", "))
    dx, dy, _ = map(int, direction.split(", "))
    return (x,y,dx,dy)

def intersect(lines: tuple[tuple[int,int,int,int]]) -> tuple[tuple[float, float], tuple[int,int,int,int], tuple[int,int,int,int]] | None:
    l1, l2 = lines
    x1, y1, dx1, dy1 = l1
    x2, y2 = x1+dx1, y1+dy1
    x3, y3, dx3, dy3 = l2
    x4, y4 = x3+dx3, y3+dy3

    denom = ((x1-x2)*(y3-y4) - (y1-y2)*(x3-x4))
    if denom == 0: return None # The lines are parallel

    px = ((x1*y2 - y1*x2)*(x3-x4) - (x1-x2)*(x3*y4 - y3*x4)) / denom
    py = ((x1*y2 - y1*x2)*(y3-y4) - (y1-y2)*(x3*y4 - y3*x4)) / denom
    return ((px, py), l1, l2)
    
def in_range(point: tuple[tuple[int,int],tuple[int,int,int,int],tuple[int,int,int,int]] | None, window: tuple[int,int,int,int]) -> bool:
    if point is None: return False # Parallel lines

    point_, l1, l2 = point
    x, y = point_

    x1, y1, x2, y2 = window
    collision_in_window = x1 <= x <= x2 and y1 <= y <= y2
    if not collision_in_window: return False

    ogx1, ogy1, velx1, vely1 = l1
    ogx2, ogy2, velx2, vely2 = l2

    # Use sign instead of division, less expensive
    collision_in_future = \
        sign(x-ogx1) == sign(velx1) and \
        sign(x-ogx2) == sign(velx2) and \
        sign(y-ogy1) == sign(vely1) and \
        sign(y-ogy2) == sign(vely2)

    return collision_in_window and collision_in_future

###########

with open("input.txt") as f:
    data = f.readlines()

lines = map(make_2d_lines, data)
combs = combinations(lines, 2)
intersections = map(intersect, combs)
# window = (7,7, 27,27)
window = (200000000000000, 200000000000000, 400000000000000, 400000000000000)
count = reduce(lambda acc, intersection: 
               acc+1 if in_range(intersection, window) else acc, 
               intersections, 0)
print(count)
