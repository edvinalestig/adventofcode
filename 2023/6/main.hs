distance :: Int -> Int -> Int
distance total press = (total-press) * press

part1 :: IO ()
part1 = do
    inp <- map (tail . words) . lines <$> readFile "input.txt"
    let [ts,ds] = map (map read) inp :: [[Int]]
        races = zip ts ds
        wins = map (\(t,d) -> length $ filter (>d) $ map (distance t) [0..t]) races
    print $ product wins

part2 :: IO ()
part2 = do
    inp <- map (concat . tail . words) . lines <$> readFile "input.txt"
    let [t,d] = map read inp :: [Int]
        wins = length $ filter (>d) $ map (distance t) [0..t]
    print wins
    