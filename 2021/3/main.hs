import Data.Char (digitToInt)

part1 :: [String] -> Int
part1 input = calc input (replicate (length $ head input) 0) (length input)

calc :: [String] -> [Int] -> Int -> Int
calc [] totals len = do
    let gamma = map (> len `div` 2) totals
    let epsilon = map not gamma
    foldl (\x y -> fromEnum y + 2*x) 0 gamma * foldl (\x y -> fromEnum y + 2*x) 0 epsilon
calc (entry:rest) totals len = calc rest (zipWith (+) totals (map digitToInt entry)) len

--------------------

part2 :: [String] -> Int
part2 input = do
    let inp = map (map digitToInt) input
    calc2 0 inp * calc3 0 inp

mostCommon :: [[Int]] -> Int -> Int
mostCommon input index = fromEnum $ foldr (\x y -> (x !! index) + y) 0 input >= ((length input + 1) `div` 2)

-- Oxygen generator
calc2 :: Int -> [[Int]] -> Int
calc2 _ [num] = foldl (\x y -> fromEnum y + 2*x) 0 num
calc2 n input = calc2 (n+1) $ filter (\x -> (x !! n) == mostCommon input n) input

-- CO2 scrubber
calc3 :: Int -> [[Int]] -> Int
calc3 _ [num] = foldl (\x y -> fromEnum y + 2*x) 0 num
calc3 n input = calc3 (n+1) $ filter (\x -> (x !! n) /= mostCommon input n) input

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = lines input 
    print "Part 1"
    print $ part1 instructions
    print "Part 2"
    print $ part2 instructions