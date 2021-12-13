import Data.Char (digitToInt)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = map digitToInt $ (concat.lines) input

    print "Part 1"
    print $ part1 instructions
    print "Part 2"
    print $ part2 instructions


part1 :: [Int] -> Int
part1 input = flashes
    where (_,flashes) = run (input,0) 100

run :: ([Int],Int) -> Int -> ([Int],Int)
run tuple 0 = tuple
run tuple n = run (step tuple) (n-1)

step :: ([Int],Int) -> ([Int],Int)
step (squid,nf) = do
    let step1 = map (+1) squid
    let (fl, numFlashes) = flashRecurse step1 []
    (flash fl, nf+numFlashes)


flashRecurse :: [Int] -> [Int] -> ([Int],Int)
flashRecurse squid flashed 
    | null flashes = (squid,length flashed)
    | otherwise = do
        let neighbours = concatMap neighbours' flashes
        let inced = incSelected squid neighbours
        flashRecurse inced (flashes ++ flashed)

    where flashes = filter (`notElem` flashed) $ getFlashes squid


incSelected :: [Int] -> [Int] -> [Int]
incSelected squid indices = zipWith (+) squid $ convertToMap indices

convertToMap :: [Int] -> [Int]
convertToMap indices = [length $ filter (==i) indices | i <- [0..99]]

getFlashes :: [Int] -> [Int]
getFlashes list = [i | i <- [0..99], list !! i > 9]

flash :: [Int] -> [Int]
flash []        = []
flash (n:rest)
    | n > 9     = 0 : flash rest
    | otherwise = n : flash rest


neighbours' :: Int -> [Int]
neighbours' num 
    | num `mod` 10 == 0 = removeBadIndices [num+1, num+10, num+11, num-9, num-10]
    | num `mod` 10 == 9 = removeBadIndices [num-1, num+10, num+9, num-10, num-11]
    | otherwise         = removeBadIndices [num-1, num+1, num-10, num-11, num-9, num+9, num+10, num+11]


removeBadIndices :: [Int] -> [Int]
removeBadIndices [] = []
removeBadIndices (index:rest)
    | index < 0 || index > 99 = removeBadIndices rest
    | otherwise               = index : removeBadIndices rest

------------------

part2 :: [Int] -> Int
part2 input = run2 input 1

run2 :: [Int] -> Int -> Int
run2 list n 
    | numFlashes == 100 = n
    | otherwise = run2 list' (n+1) 
    where 
        (list', numFlashes) = step2 list

step2 :: [Int] -> ([Int],Int)
step2 squid = do
    let step1 = map (+1) squid
    let (fl, numFlashes) = flashRecurse step1 []
    (flash fl, numFlashes)