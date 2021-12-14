import Data.List.Split (splitOn)
import Data.List (sort)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = map read $ splitOn "," input 
    
    print "Part 1"
    print $ part1 instructions
    print "Part 2"
    print $ part2 instructions

part1 :: [Int] -> Int
part1 ns = sum [abs (n - med) | n <- ns, let med = median ns]

median :: [Int] -> Int
median ns = sort ns !! (length ns `div` 2)

part2 :: [Int] -> Int
part2 ns = minimum [bruteForce ns i | i <- [1..1000]]

bruteForce :: [Int] -> Int -> Int
bruteForce ns target = sum [sumDiff n target | n <- ns]

sumDiff :: Int -> Int -> Int
sumDiff start end = (abs (end-start) * (abs (end-start) + 1)) `div` 2