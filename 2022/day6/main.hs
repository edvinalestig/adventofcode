import Data.List (nub)

part1 :: IO ()
part1 = do
    inp <- readFile "input.txt"
    print $ getFirst inp 4 0

part2 :: IO ()
part2 = do
    inp <- readFile "input.txt"
    print $ getFirst inp 14 0

getFirst :: String -> Int -> Int -> Int
getFirst inp n i = if (length . nub . take n) inp == n then i+n else getFirst (tail inp) n (i+1)
