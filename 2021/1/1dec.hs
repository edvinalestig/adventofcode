import System.IO

-- Part 1

countIncreases :: [Int] -> Int
countIncreases (x1:x2:xs) 
    | x1 < x2    = 1 + countIncreases (x2:xs)
    | otherwise  = countIncreases (x2:xs)
countIncreases _ = 0

part2 :: [Int] -> Int
part2 (x1:x2:x3:x4:xs)
    | (x1+x2+x3) < (x2+x3+x4) = 1 + part2 (x2:x3:x4:xs)
    | otherwise               = part2 (x2:x3:x4:xs)
part2 _                       = 0

main :: IO ()
main = do
    input <- readFile "1dec.txt"
    let ints = (map read . words) input
    print "Part 1"
    print $ countIncreases ints
    print "Part 2"
    print $ part2 ints