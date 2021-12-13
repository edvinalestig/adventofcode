import Data.IntMap.Lazy
    ( IntMap, fromList, insert, map, member, (!?) )
import Data.List (elemIndex)
import Data.Maybe (fromJust)

inputNumbers :: [Int]
inputNumbers = reverse [0,20,7,16,1,18,15]

exampleNumbers :: [Int]
exampleNumbers = reverse [0,3,6]

main :: IO ()
main = run2

-- O (n^2)
part1 :: Int -> [Int] -> Int
part1 i input 
    | i == 2020 = head input
    | otherwise = do
        let c = countPast1 (head input) (tail input)
        if c == 0 then
            part1 (i+1) (0:input)
        else
            part1 (i+1) (c:input)

countPast1 :: Int -> [Int] -> Int
countPast1 match (x:xs)
    | x == match = 1
    | Prelude.null xs    = 0
    | otherwise  = do
        let c = countPast1 match xs
        if c == 0 then 0
        else c + 1


-- ~ O(n)
part2v3 :: Int -> Int -> IntMap Int -> Int
part2v3 i lastSaid list
    | i >= 30000000 - 1 = lastSaid
    | otherwise = do
        case list !? lastSaid of
            Nothing    -> do  -- not in the map, say 0
                part2v3 (i+1) 0         (insert lastSaid i list)
            Just point -> do  -- in the map, say turns apart (now - then)
                part2v3 (i+1) (i-point) (insert lastSaid i list)


run :: IO ()
run = print (part1 (length inputNumbers) inputNumbers)

run2 :: IO ()
run2 = do
    let num = inputNumbers
    let intm = toIntMapv2 num
    -- print intm
    print (part2v3 (length num-1) (head num) intm)

-- run2v2 :: IO ()
-- run2v2 = do
--     print (part2v2 (length inputNumbers) inputNumbers (toIntMap2 inputNumbers))

toIntMapv2 :: [Int] -> IntMap Int
toIntMapv2 list = fromList (tail [(x, fromJust (elemIndex x (reverse list))) | x <- list])

-- toIntMap2 :: [Int] -> IntMap ()
-- toIntMap2 list = fromList [(x, ()) | x <- list]