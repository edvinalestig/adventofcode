
run :: ([Int] -> Int) -> IO ()
run f = do
    inp <- map (map read . words) . lines <$> readFile "input.txt" :: IO [[Int]]
    print . sum $ map f inp

part1 :: IO ()
part1 = run oasis

oasis :: [Int] -> Int
oasis ns
    | all (==0) ns = 0
    | otherwise = last ns + oasis (diffs ns)

diffs :: [Int] -> [Int]
diffs [_]        = []
diffs (n1:n2:ns) = n2-n1 : diffs (n2:ns)

-- Part 2
part2 :: IO ()
part2 = run oasis2

oasis2 :: [Int] -> Int
oasis2 ns
    | all (==0) ns = 0
    | otherwise = head ns - oasis2 (diffs ns)
