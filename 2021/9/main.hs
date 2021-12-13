import Data.List (delete, nub)
import Data.Char (digitToInt)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = lines input

    -- print "Part 1"
    -- print $ part1 instructions
    print "Part 2"
    print $ part2 instructions

part1 :: [String] -> Int
part1 input = sum [1 + digitToInt comp | row <- [0..99], col <- [0..99], let comp = (input !! row) !! col,
                   left input row col comp, right input row col comp,
                   bottom input row col comp, top input row col comp]

top :: [String] -> Int -> Int -> Char -> Bool
top _ 0 _ _ = True
top input row col comp = (input !! (row-1)) !! col > comp

right :: [String] -> Int -> Int -> Char -> Bool
right _ _ 99 _ = True
right input row col comp = (input !! row) !! (col+1) > comp

bottom :: [String] -> Int -> Int -> Char -> Bool
bottom _ 99 _ _ = True
bottom input row col comp = (input !! (row+1)) !! col > comp

left :: [String] -> Int -> Int -> Char -> Bool
left _ _ 0 _ = True
left input row col comp = (input !! row) !! (col-1) > comp

---------------------

data Coord = Coord Int Int deriving (Eq,Ord,Show)
type Edge = [Coord]

part2 :: [String] -> Int
part2 input = threeMaximum $ part2' $ createEdges input

part2' :: [Edge] -> [Int]
part2' []    = []
part2' edges = do
    let (remEdges,basinSize) = run edges (head $ head edges) 
    basinSize : part2' remEdges

run :: [Edge] -> Coord -> ([Edge], Int)
run edges coord = do
    let visited = nub $ search edges [] coord
    let remainingEdges = filterEdges visited edges
    (remainingEdges, length visited)

search :: [Edge] -> [Coord] -> Coord -> [Coord]
search    edges     visited  currentPos 
    | currentPos `elem` visited = visited
    | null next = currentPos:visited
    | otherwise = startSearch edges (currentPos:visited) next
    where 
        next = filter (`notElem` visited) $ getNext edges currentPos

startSearch :: [Edge] -> [Coord] -> [Coord] -> [Coord]
startSearch    _         visited    []          = visited
startSearch    edges     visited    (c:toVisit) = do
    let visited' = nub $ visited ++ search edges visited c
    startSearch edges visited' (filter (`notElem` visited') toVisit)

filterEdges :: [Coord] -> [Edge] -> [Edge]
filterEdges coords = filter (\[x,y] -> x `notElem` coords && y `notElem` coords)

getNext :: [Edge] -> Coord -> [Coord]
getNext edges coord = [if c1 == coord then c2 else c1 | [c1,c2] <- edges, c1 == coord || c2 == coord]

createEdges :: [String] -> [Edge]
createEdges input = concatMap (getNeighbours input) [Coord x y | y <- [0..99], x <- [0..99]]

getNeighbours :: [String] -> Coord -> [Edge]
getNeighbours input coord 
    | get' input coord == '9' = []
    | r `notElem` ['.','9'] && b `notElem` ['.','9'] = [[coord, incX coord], [coord, incY coord]]
    | r `notElem` ['.','9'] = [[coord, incX coord]]
    | b `notElem` ['.','9'] = [[coord, incY coord]]
    | otherwise = []
    where
        (r,b) = (right' input coord, bottom' input coord)

right' :: [String] -> Coord -> Char
right' _     (Coord 99  _  )  = '.'
right' input (Coord col row) = (input !! row) !! (col+1)

bottom' :: [String] -> Coord -> Char
bottom' _     (Coord _   99 )   = '.'
bottom' input (Coord col row) = (input !! (row+1)) !! col

incY :: Coord -> Coord
incY (Coord _   99 ) = error "No more rows (end)"
incY (Coord col row) = Coord col (row+1)

incX :: Coord -> Coord
incX (Coord 99  _  ) = error "No more columns (end)"
incX (Coord col row) = Coord (col+1) row

get' :: [String] -> Coord -> Char
get' input (Coord col row) = (input !! row) !! col

threeMaximum :: [Int] -> Int
threeMaximum input
    | length input < 3 = error "List too short"
    | length input == 3 = product input
    | otherwise = do
        let max1 = maximum input
        let max2 = maximum $ delete max1 input
        let max3 = maximum $ delete max1 $ delete max2 input
        max1*max2*max3
