{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List.Split (splitOn)
import Data.List (nub)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let [dots, folds] = map lines $ splitOn "\n\n" input
    let dots'  = map ((map read :: [String] -> [Int]) . splitOn ",") dots
    let folds' = map ((\[xy,n] -> (xy, read n :: Int)) . splitOn "=" . drop 11) folds

    print "Part 1"
    print $ part1 (dots',folds')
    print "Part 2"
    putStrLn $ part2 (dots',folds')

part1 :: ([[Int]], [(String,Int)]) -> Int
part1 (dots,f:_) = length $ foldPage dots f

foldPage :: [[Int]] -> (String,Int) -> [[Int]]
foldPage page (dir,coord) 
    | dir == "x" = nub $ map (fx coord) page
    | dir == "y" = nub $ map (fy coord) page

fx :: Int -> [Int] -> [Int]
fx xCoord [x,y]
    | x > xCoord = [2*xCoord-x, y]
    | otherwise  = [x, y]

fy :: Int -> [Int] -> [Int]
fy yCoord [x,y]
    | y > yCoord = [x, 2*yCoord-y]
    | otherwise  = [x, y]

------------------

part2 :: ([[Int]], [(String,Int)]) -> String
part2 (dots,folds) = printFancy $ foldl foldPage dots folds

printFancy :: [[Int]] -> String
printFancy dots = unlines [[if b then '■' else ' ' | x <- [0..38], let b = [x,y] `elem` dots] | y <- [0..5]]
