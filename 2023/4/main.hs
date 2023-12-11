import Data.List (intersect)
import qualified Data.Map as M

parse :: String -> (Int,[Int],[Int])
parse s = do
    let cardNum = read . last . words $ takeWhile (/=':') s :: Int
    let next    = tail $ dropWhile (/=':') s
    let winning = map read . words        $ takeWhile (/='|') next :: [Int]
    let nums    = map read . words . tail $ dropWhile (/='|') next :: [Int]
    (cardNum, winning, nums)

main :: IO ()
main = do
    inp <- lines <$> readFile "input.txt"
    let cards = map parse inp
    let intersections = map (\(c,w,n) -> (c,length $ w `intersect` n)) cards
    
    -- Part 1
    let points = sum $ map (\(_,l) -> if l == 0 then 0 else 2^(l-1)) intersections
    putStrLn $ "Part 1: " ++ show points

    -- Part 2
    let cardTotals = M.fromList [(c,1) | (c,_) <- intersections]
    let totals = foldl combine cardTotals intersections
    putStrLn $ "Part 2: " ++ show (M.foldl (+) 0 totals)

combine :: M.Map Int Int -> (Int,Int) -> M.Map Int Int
combine totals (c,n) = M.unionWith (+) totals $ M.fromList [(cc, totals M.! c) | cc <- [c+1 .. c+n]]
