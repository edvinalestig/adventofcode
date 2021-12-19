import Data.List.Split (splitOn)

type Target = [[Int]]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let target = (map ((map (read :: String -> Int)) . (splitOn ".."))) . (splitOn ", y=") . (drop 15) $ input

    print input
    let (p1,p2) = part1and2 target

    putStrLn "-- Part 1 --"
    -- print $ part1 target 
    print p1
    putStrLn "-- Part 2 --"
    -- print $ part2 target
    print p2

isWithin :: (Int,Int) -> Target -> Bool
isWithin (x,y) [[x1,x2],[y1,y2]] = x >= x1 && x <= x2 && y >= y1 && y <= y2

past :: (Int,Int) -> Target -> Bool
past (x,y) [[_,x2],[y1,_]] = x > x2 || y < y1

step :: ((Int,Int),(Int,Int)) -> ((Int,Int),(Int,Int))
step ((x,y),(xVel,yVel)) = ((x + xVel, y + yVel), (xVel - signum xVel, yVel - 1))

launch :: (Int,Int) -> Target -> Int
launch vel target = do
    let (points, hit) = launch' ((0,0),vel) target 
    if hit then maximum points
    else -999999

launch' :: ((Int,Int),(Int,Int)) -> Target -> ([Int],Bool)
launch' ((x,y),vel) target
    | isWithin (x,y) target = ([y],True)
    | past (x,y) target = ([],False)
    | otherwise = (y : list, b)
    where (list,b) = launch' (step ((x,y),vel)) target

-- part1 :: Target -> Int
-- part1 [[x1,x2],ys] = maximum [launch (x,y) [[x1,x2],ys] | x <- [0..x2], y <- [0..10000]]

-- part2 :: Target -> Int
-- part2 [[x1,x2],[y1,y2]] = length $ filter (>(-99999)) [launch (x,y) [[x1,x2],[y1,y2]] | x <- [0..x2], y <- [y1..6000]]

-- Uses brute force
part1and2 :: Target -> (Int,Int)
part1and2 [[x1,x2],[y1,y2]] = (maximum list, length $ filter (>(-99999)) list)
    where list = [launch (x,y) [[x1,x2],[y1,y2]] | x <- [0..x2], y <- [y1..6000]]
