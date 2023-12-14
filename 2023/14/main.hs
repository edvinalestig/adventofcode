import Data.List (transpose)
import Data.List.Split (split, onSublist)

part1 :: IO ()
part1 = do
    inp <- transpose . lines <$> readFile "input.txt"
    let plane = map (filter (not . null) . split (onSublist "#")) inp
    let load = map countNorthTilt plane
    print $ sum load

countNorthTilt :: [String] -> Int
countNorthTilt [] = 0
countNorthTilt (s:ss)
    | 'O' `notElem` s = countNorthTilt ss
    | otherwise = do
        let n = length $ filter (=='O') s
        let empty = length s - n
        let len = sum $ map length ss
        (len+empty)*n + sum [1..n] + countNorthTilt ss

--------------------------

part2 :: IO ()
part2 = do
    inp <- lines <$> readFile "input.txt"
    let (n,loop) = findCycle [] inp
    let billionth = loop !! ((1000000000 - n) `rem` length loop)
    print $ countLoad billionth

countLoad :: [String] -> Int
countLoad plane = do
    let zipped = map (\s -> zip s $ reverse [1..length s]) $ transpose plane
    sum $ map (foldl (\acc (c,n) -> if c == 'O' then acc + n else acc) 0) zipped

findCycle :: [[String]] -> [String] -> (Int,[[String]])
findCycle prevs plane
    | plane `elem` prevs = do
        let prevs' = reverse prevs
        let n = length $ takeWhile (/=plane) prevs'
        (n, drop n prevs')
    | otherwise = findCycle (plane:prevs) $ doCycle plane

-- Tilt a single row/column
tilt :: String -> String
tilt s = concat $ tilt' $ split (onSublist "#") s
    where
        tilt' :: [String] -> [String]
        tilt' [] = []
        tilt' (s:ss)
            | 'O' `notElem` s = s : tilt' ss
            | otherwise = do
                let n = length $ filter (=='O') s
                let empty = length s - n
                (replicate n 'O' ++ replicate empty '.') : tilt' ss

doCycle :: [String] -> [String]
doCycle plane = do
    let north = map tilt $ transpose plane
    let west  = map tilt $ transpose north
    let south = map (reverse . tilt . reverse) $ transpose west
    map (reverse . tilt . reverse) $ transpose south -- east