{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}
import Data.List.Split ( splitOn )
import Data.List ( group, sort )

type Pattern = [String]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let [polymer, patterns] = splitOn "\n\n" input
    let patterns' = map (splitOn " -> ") $ lines patterns

    print "Part 1"
    -- let p1 = part1 patterns' polymer
    -- print $ count (mostCommon p1) p1 - count (leastCommon p1) p1
    print $ part1v2 patterns' polymer 
    print "Part 2"
    print $ part2 patterns' polymer


---- Bad and inefficent ----

-- part1 :: [Pattern] -> String -> String
-- part1 patterns polymer = part1' patterns polymer 10

-- part1' :: [Pattern] -> String -> Int -> String
-- part1' _        polymer 0 = polymer
-- part1' patterns polymer i = part1' patterns (insertPolymer patterns polymer) (i-1)

-- -- Too inefficient for part 2 :(
-- insertPolymer :: [Pattern] -> String -> String
-- insertPolymer _ []  = []
-- insertPolymer _ [c] = [c]
-- insertPolymer patterns (c1:c2:rest) = (c1:getPoly patterns c1 c2) ++ insertPolymer patterns (c2:rest)

-- Get what should be inserted between the two characters
getPoly :: [Pattern] -> Char -> Char -> String
getPoly []           _  _  = []
getPoly ([poly,new]:patterns) c1 c2 
    | [c1,c2] == poly = new
    | otherwise       = getPoly patterns c1 c2

-- count :: Eq a => a -> [a] -> Int
-- count item = length . filter (==item) 


-- -- Stolen from https://www.reddit.com/r/haskell/comments/7u1gay/finding_the_most_common_element_in_a_list/dtgvlbp/:
-- mostCommon :: String -> Char
-- mostCommon = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

-- leastCommon :: String -> Char
-- leastCommon = snd . minimum . map (\xs -> (length xs, head xs)) . group . sort

-- -- End of stolen

-- Run the code from part 2 instead
part1v2 :: [Pattern] -> String -> Int
part1v2 patterns polymer = maximum list - minimum list
    where 
        list = map snd $ convertToLetters (([last polymer], 1) : part2' patterns (createTuples polymer) 10)

---- Part 2 ----

-- list is [Int] with all the charcter counts. The last character in the input value has to be added manually.
part2 :: [Pattern] -> String -> Int
part2 patterns polymer = maximum list - minimum list
    where 
        list = map snd $ convertToLetters (([last polymer], 1) : part2' patterns (createTuples polymer) 40)

-- Go from pairs of letters to single letters. "BN" -> "B" The second letter is the first letter in another pair.
convertToLetters :: [(String,Int)] -> [(String,Int)]
convertToLetters list = combine $ map (\(s,n) -> ([head s],n)) list

-- Create tuples with pairs of characters
createTuples :: String -> [(String,Int)]
createTuples [c1,c2]      = [([c1,c2], 1)]
createTuples (c1:c2:rest) = ([c1,c2], 1) : createTuples (c2:rest)

-- Recursive function to iterate n times. Calls run each iteration.
part2' :: [Pattern] -> [(String,Int)] -> Int -> [(String,Int)]
part2' _        polymer 0 = polymer
part2' patterns polymer n = part2' patterns (run patterns polymer) (n-1)

-- Does one iteration of expanding.
run :: [Pattern] -> [(String,Int)] -> [(String,Int)] 
run patterns list = combine $ run' patterns list

-- Performes the expansion. Expands each pair individually.
run' :: [Pattern] -> [(String,Int)] -> [(String,Int)]
run' _        []           = []
run' patterns (entry:rest) = c patterns entry ++ run' patterns rest

-- Goes from one pair to two pairs. "BN" where "H" should be inserted becomes "BH","HN" with the same counts.
c :: [Pattern] -> (String,Int) -> [(String,Int)]
c patterns (pair,num) = map (,num) $ getInsertion patterns pair

-- Combines multiple entries of the same pair into one with the sum of all counts.
combine :: [(String,Int)] -> [(String,Int)]
combine = foldl combine' [] 

-- Checks if the pair equals the pair in the list, if so sum the counts.
combine' :: [(String,Int)] -> (String,Int) -> [(String,Int)]
combine' [] new = [new]
combine' ((pair,n):condensed) (newPair,newN)
    | pair == newPair = (pair, n+newN) : condensed
    | otherwise       = (pair,n) : combine' condensed (newPair,newN)

-- Gets the two new pairs from the input pair. "BN" -> ["BH","HN"]
getInsertion :: [Pattern] -> [Char] -> [String]
getInsertion patterns [c1,c2] 
    | null gp = []
    | otherwise       = [c1:gp, gp ++ [c2]]
    where gp = getPoly patterns c1 c2
