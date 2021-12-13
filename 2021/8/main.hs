import Data.List ((\\), intersect, sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = map (\x -> map (splitOn " ") $ splitOn " | " x) $ lines input

    print "Part 1"
    print $ part1 input
    print "Part 2"
    print $ part2 instructions 0
    print $ part2' instructions

-- Oneliner :)
part1 :: String -> Int                               --                            instructions -->
part1 input = length $ filter (\x -> length x /= 6 && length x /= 5) $ concat $ map (\[_,x] -> x) $ map (\x -> map (splitOn " ") $ splitOn " | " x) $ lines input

-- Length 2: 1
-- Length 3: 7
-- Length 4: 4
-- Length 5: 2, 3, 5
-- Length 6: 0, 6, 9
-- Length 7: 8

-- Debug version
part2' :: [[[String]]] -> [Int]
part2' = map translate


-- Yay tail recursion
part2 :: [[[String]]] -> Int -> Int
part2 []           total = total
part2 (entry:list) total = part2 list $! total + translate entry

translate :: [[String]] -> Int
translate [pattern, output] = read $ map (convertDigit pattern) output
translate _ = -1

convertDigit :: [String] -> String -> Char
convertDigit pattern digit = case length digit of
    2 -> '1'
    3 -> '7'
    4 -> '4'
    5 -> twoThreeOrFive pattern digit
    6 -> zeroSixOrNine pattern digit
    7 -> '8'
    _ -> '.'

-- -- Length 5
-- twoThreeOrFive :: [String] -> String -> Char
-- twoThreeOrFive pattern digit 
--     | length (digit \\ (head $ filter (\x -> length x == 2) pattern)) == 3 = '3' -- Remove all segments in "1". 3 segs left: 3
--     | length (digit \\ (head $ filter (\x -> length x == 4) pattern)) == 2 = '5' -- Remove all segments in "4".
--     | length (digit \\ (head $ filter (\x -> length x == 4) pattern)) == 3 = '2'
--     -- | otherwise = '2'                                                            -- 2 segments left: 5, otherwise 2

-- Length 6
-- sixOrNine :: [String] -> String -> Char
-- sixOrNine pattern digit 
--     | length (digit \\ (head $ filter (\x -> length x == 2) pattern)) == 5 = '6' -- Remove all segments in "1". 
--     | length (digit \\ (head $ filter (\x -> length x == 2) pattern)) == 4 = '9'
--     -- | otherwise = '9'                                                            -- 5 segments left: 6, otherwise 9

zeroSixOrNine :: [String] -> String -> Char
zeroSixOrNine pattern digit
    | head (filter (\x -> length x == 4) pattern) `subsetOf` digit = '9' -- digit subset of 4
    | head (filter (\x -> length x == 3) pattern) `subsetOf` digit = '0' -- digit subset of 7
    | otherwise = '6'

twoThreeOrFive :: [String] -> String -> Char
twoThreeOrFive pattern digit
    | head (filter (\x -> length x == 3) pattern) `subsetOf` digit = '3'
    | digit `subsetOf` head (filter (\x -> length x == 6 && zeroSixOrNine pattern x == '9') pattern) = '5'
    | otherwise = '2'

-----------------

subsetOf :: String -> String -> Bool
a `subsetOf` b = null [x | x <- a, x `notElem` b]

twoThreeOrFive' :: [String] -> String -> Char
twoThreeOrFive' pattern digit
    | head (filter (\x -> length x == 3) pattern) `subsetOf` digit = '3' -- 7 subset of digit
    | True = '5'
    | otherwise = '2'
-- | digit `subsetOf` (head $ filter (\x -> length x == 6) pattern) = '5' -- Digit subset of 6 or 9
-- | length (digit \\ (head $ filter (\x -> length x == 4) pattern)) == 2 = '5'
-- | isFive pattern digit = '5'

-- sixOrNine' :: [String] -> String -> Char
-- sixOrNine' pattern digit
--     | (head $ filter (\x -> length x == 4) pattern) `subsetOf` digit = '9'
--     | otherwise = '6'

-- -- (4 - 1) subset of 5
-- isFive :: [String] -> String -> Bool
-- isFive pattern digit = ((head $ filter (\x -> length x == 4) pattern) \\ (head $ filter (\x -> length x == 2) pattern)) `subsetOf` digit