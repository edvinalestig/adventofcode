import Data.List.Split (splitOn)
import Data.List (group)

part1 :: IO ()
part1 = do
    inp <- map parse . lines <$> readFile "input.txt"
    print $ sum $ map countArrangements inp
    where
        parse :: String -> (String,[Int])
        parse s = do
            let [str,ns] = words s
            (str, map read $ splitOn "," ns)

countArrangements :: (String,[Int]) -> Int
countArrangements (s, nums) = do
    let strings = createStrings s
    let groups  = map (map length . filter (elem '#') . group) strings
    length $ filter (==nums) groups

createStrings :: String -> [String]
createStrings ""      = [""]
createStrings ('?':s) = do
    let next = createStrings s
    map ('.':) next ++ map ('#':) next
createStrings (c:s) = map (c:) $ createStrings s
