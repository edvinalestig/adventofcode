import Data.List.Split (splitOn)
import Data.List (nub)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = lines input 
    let coords = map (map (\x -> map (read) $ splitOn "," x)) $ map (splitOn " -> ") instructions :: [[[Int]]]
    
    print "Part 1"
    print $ part1 $ filterDiagonal coords
    print "Part 2"
    print $ part1 coords

part1 :: [[[Int]]] -> Int
part1 coords = length $ nub $ concat [compareLines l1 l2 | l1 <- coords, l2 <- coords, l1 /= l2]

filterDiagonal :: [[[Int]]] -> [[[Int]]]
filterDiagonal [] = []
filterDiagonal ([[x1,y1],[x2,y2]]:rest) 
    | x1 /= x2 && y1 /= y2 = filterDiagonal rest
    | otherwise = [[x1,y1],[x2,y2]]:(filterDiagonal rest)

between :: Int -> Int -> Int -> Bool
between val start end = (min start end) <= val && (max start end) >= val

same :: Int -> Int -> Int -> Int -> Bool
same p1 p2 p3 p4 = p1 == p2 && p2 == p3 && p3 == p4

overlap :: Int -> Int -> Int -> Int -> [Int]
overlap p1 p2 p3 p4 
    | (between p3 p1 p2) && (between p4 p1 p2) = [(min p3 p4)..(max p3 p4)]
    | (between p1 p3 p4) && (between p2 p3 p4) = [(min p1 p2)..(max p1 p2)]
    | p3 >  p4 && between p3 p1 p2 = [(min p1 p2)..p3]
    | p3 <= p4 && between p3 p1 p2 = [p3..(max p1 p2)]
    | p3 >  p4 && between p4 p1 p2 = [p4..(max p1 p2)]
    | p3 <= p4 && between p4 p1 p2 = [(min p1 p2)..p4]

diagonal :: [[Int]] -> Bool
diagonal [[x1,y1],[x2,y2]] = x1 /= x2 && y1 /= y2

compareLines :: [[Int]] -> [[Int]] -> [(Int,Int)]
compareLines [[x1,y1],[x2,y2]] [[x3,y3], [x4,y4]]
    | not $ (between x3 x1 x2) || (between x4 x1 x2) || (between x1 x3 x4) || (between x2 x3 x4) = [] -- No overlap
    | not $ (between y3 y1 y2) || (between y4 y1 y2) || (between y1 y3 y4) || (between y2 y3 y4) = [] -- No overlap
    | (diagonal [[x1,y1],[x2,y2]]) && (diagonal [[x3,y3], [x4,y4]]) = compareDiagonal [[x1,y1],[x2,y2]] [[x3,y3], [x4,y4]]
    | (diagonal [[x1,y1],[x2,y2]]) || (diagonal [[x3,y3], [x4,y4]]) = compareStraightDiagonal [[x1,y1],[x2,y2]] [[x3,y3], [x4,y4]]
    | same y1 y2 y3 y4 = [(x,y1) | x <- (overlap x1 x2 x3 x4)] -- Both horizontal, calculate x-overlap
    | same x1 x2 x3 x4 = [(x1,y) | y <- (overlap y1 y2 y3 y4)] -- Both vertical, calculate y-overlap
    | x1 == x2         = [(x1,y3)] -- Crossing at (x1=x2,y3=y4)
    | y1 == y2         = [(x3,y1)] -- Crossing at (x3=x4,y1=y2)

-- Both lines diagonal
compareDiagonal :: [[Int]] -> [[Int]] -> [(Int,Int)]
compareDiagonal line1 line2
    | k' line1 /= k' line2 = [(head int, head $ tail int) | let int = intersection line1 line2, 
                              let p1 = compareLines line1 [int,int], let p2 = compareLines line2 [int,int],
                              p1 == p2, p1 /= [] ] -- Orthogonal
    | m' line1 /= m' line2 = [] -- Parallel but not the same line.
    | otherwise = overlapDiagonal line1 line2 -- The same line

-- x = (m2-m1)/(k1-k2)
-- y = (k1m2-k2m1)/(k1-k2)
-- -> [x,y]
intersection :: [[Int]] -> [[Int]] -> [Int]
intersection line1 line2 = [((m' line2) - (m' line1)) `div` ((k' line1) - (k' line2)), 
    ((k' line1)*(m' line2) - (k' line2)*(m' line1)) `div` ((k' line1) - (k' line2))]

overlapDiagonal :: [[Int]] -> [[Int]] -> [(Int,Int)]
overlapDiagonal [[x1,y1],[x2,y2]] [[x3,y3], [x4,y4]] = [(x, y' [[x3,y3], [x4,y4]] x) | x <- overlap x1 x2 x3 x4]

-- One line diagonal
compareStraightDiagonal :: [[Int]] -> [[Int]] -> [(Int,Int)]
compareStraightDiagonal line1 line2
    | diagonal line1 = compareStraightDiagonal' line1 line2
    | diagonal line2 = compareStraightDiagonal' line2 line1

-- 1st line is diagonal, 2nd is straight
compareStraightDiagonal' :: [[Int]] -> [[Int]] -> [(Int,Int)]
compareStraightDiagonal' line1 [[x3,y3], [x4,y4]]
    | x3 == x4 = compareLines [[x3, y' line1 x3],[x3, y' line1 x3]] [[x3,y3], [x4,y4]]
    | y3 == y4 = compareLines [[x' line1 y3, y3],[x' line1 y3, y3]] [[x3,y3], [x4,y4]]

-- k = (y2-y1)/(x2-x1)
k' :: [[Int]] -> Int
k' [[x1,y1],[x2,y2]] = (y2-y1) `div` (x2-x1)

-- m = y1 - k*x1
m' :: [[Int]] -> Int
m' [[x1,y1],xy2] = y1 - (k' [[x1,y1],xy2])*x1

-- y = kx+m
y' :: [[Int]] -> Int -> Int
y' ln x = (k' ln) * x + (m' ln)

-- x = (y-m)/k
x' :: [[Int]] -> Int -> Int
x' ln y = (y - (m' ln)) `div` (k' ln)

