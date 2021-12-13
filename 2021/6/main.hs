import Data.List.Split (splitOn)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let fish = map (read) $ splitOn "," input 
    
    print "Part 1"
    print $ run fish 80
    print "Part 2"
    print $ run fish 256

run :: [Int] -> Int -> Int
run fish i = foldr (\(_,n) m -> n+m) 0 $ part2 (convert fish) i

count :: Int -> [Int] -> Int
count n list = sum [1 | i <- list, i == n]

combine :: Int -> [(Int,Int)] -> (Int,Int)
combine n list = (n, sum [m | (age,m) <- list, age == n])

convert :: [Int] -> [(Int,Int)]
convert fish = [(i,n) | i <- [0..6], let n = count i fish]

part2 :: [(Int,Int)] -> Int -> [(Int,Int)]
part2 fish 0 = fish
part2 fish n = part2 ([combine i list | i <- [0..8], let list = part2' fish]) (n-1)

part2' :: [(Int,Int)] -> [(Int,Int)]
part2' [] = []
part2' ((0,n):rest)   = (6,n):(8,n):(part2' rest)
part2' ((age,n):rest) = (age-1,n):(part2' rest)