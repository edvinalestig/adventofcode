import Data.List
import Data.IntMap.Lazy
import Data.Maybe (fromJust)

main = do
    part1
    part2


part1 :: IO ()
part1 = do
    input <- readFile "10dec.txt"
    let ints = 0 : sort [read n :: Int | n <- words input]
    let intArray = pair (ints ++ [maximum ints + 3])
    let diff1 = length [1 | (x:y:_) <- intArray, y-x == 1]
    let diff3 = length [1 | (x:y:_) <- intArray, y-x == 3]
    print (diff3 * diff1)

pair :: [Int] -> [[Int]]
pair [x1, x2]   = [[x1, x2]]
pair (x1:x2:xs) = [x1, x2] : pair (x2:xs)
pair _           = []

part2 :: IO ()
part2 = do
    input <- readFile "10dec.txt"
    let ints = 0 : sort [read n :: Int | n <- words input]
    let graphs = [pair5 list | list <- subInts ints]
    print (product [length (connect g (minimum (keys g)) (maximum (keys g))) | g <- graphs])

pair5 :: [Int] -> IntMap [Int]
pair5 ints = fromList [(x, [y | y <- ints, y > x, y - x <= 3]) | x <- ints]

subInts :: [Int] -> [[Int]]
subInts [] = []
subInts ints
    | not (Data.List.null arr) = (arr ++ [maximum arr + 1]) : subInts (drop (fromJust (elemIndex (maximum arr) ints)+1) ints)
    | otherwise = subInts (tail ints)
    where arr = takeWhile (\x -> ((x+1) `elem` ints) || ((x+2) `elem` ints)) ints

connect :: IntMap [Int] -> Int -> Int -> [[(Int, Int)]]
connect graph x y
    | x == y    = [[]]
    | otherwise = [(x,t):p | t <- graph!x, p <- connect graph t y]
