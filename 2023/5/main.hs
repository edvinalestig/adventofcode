import Data.Text (splitOn, pack, unpack)
import Data.List.Split (chunksOf)

-- [dest, source, length]
mapValue :: Int -> [[Int]] -> Int
mapValue n [] = n
mapValue n ([d,s,l]:maps)
    | n >= s && n < s+l = d + n - s
    | otherwise = mapValue n maps

part1 :: IO ()
part1 = do
    inp <- readFile "input.txt"
    let (seeds':maps') = map unpack <$> splitOn (pack "\n\n") $ pack inp
        (_:seeds) = map read $ words seeds' :: [Int]
        maps = map (map (map read . words) . tail . lines) maps' :: [[[Int]]]
        locations = map (\s -> foldl mapValue s maps) seeds
    print $ minimum locations

----------- Part 2 -----------

part2 :: IO () -- SLOOOOOOOOOOOOOOOOOOW
part2 = do
    inp <- readFile "input.txt"
    let (seeds':maps') = map unpack <$> splitOn (pack "\n\n") $ pack inp
        (_:seeds'') = map read $ words seeds' :: [Int]
        seeds = concatMap range $ chunksOf 2 seeds''
        maps = map (map (map read . words) . tail . lines) maps' :: [[[Int]]]
        locations = map (\s -> foldl mapValue s maps) seeds
    print $ minimum locations
    where
        range [a,b] = [a..a+b-1]

mapRange :: [[Int]] -> (Int,Int) -> [(Int,Int)]
mapRange [] range = [range]
mapRange ([d,s,l]:maps) (a,b)
    | a >= s && b < s+l = [(d+a-s, d+b-s)]                      -- Wholly contained
    | a >= s && a < s+l = (d+a-s, d+l) : mapRange maps (s+l, b) -- Overflows
    | b >= s && b < s+l = (d, d+b-s)   : mapRange maps (a, s-1) -- Underflows
    | otherwise         = mapRange maps (a, b)                  -- Uncontained

part2' :: IO () -- Fast!
part2' = do
    inp <- readFile "input.txt"
    let (seeds':maps') = map unpack <$> splitOn (pack "\n\n") $ pack inp
        seedRanges = map (\[a,b] -> (a,a+b-1)) . chunksOf 2 . tail . map read $ words seeds' :: [(Int, Int)]
        maps = map (map (map read . words) . tail . lines) maps' :: [[[Int]]]
        locations = foldl (\ranges m1 -> concatMap (mapRange m1) ranges) seedRanges maps
    print $ minimum $ map fst locations

