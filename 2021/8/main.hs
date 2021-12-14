import Data.List.Split (splitOn)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = map (map (splitOn " ") . splitOn " | ") $ lines input

    print "Part 1"
    print $ part1 input
    print "Part 2"
    print $ part2 instructions 0

-- Oneliner :)
part1 :: String -> Int
part1 input = length $ filter (\x -> length x /= 6 && length x /= 5) $ concatMap ((\[_,x] -> x) . (map (splitOn " ") . splitOn " | ")) $ lines input

-- Length 2: 1
-- Length 3: 7
-- Length 4: 4
-- Length 5: 2, 3, 5
-- Length 6: 0, 6, 9
-- Length 7: 8

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

subsetOf :: Eq a => [a] -> [a] -> Bool
a `subsetOf` b = null [x | x <- a, x `notElem` b]
