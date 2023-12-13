import Data.List (intersect)
import qualified Data.Map as M
import Text.Regex (matchRegex, mkRegex)

parse :: String -> (Int,[Int],[Int])
parse s = do
    let rx = mkRegex "Card +([0-9]+): ([0-9 ]+) \\| ([0-9 ]+)"
    let Just [cardnum,wins,mynums] = matchRegex rx s
    (read cardnum,map read $ words wins,map read $ words mynums)

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
