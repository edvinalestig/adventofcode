import Data.List.Split

data Cell = Cell Int Bool deriving (Show, Eq)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = lines input 
    let numbers = map (read) $ splitOn "," $ head instructions
    let boards  = map (map (\x -> Cell (read x) False)) $ split' $ filter (/="") $ tail instructions :: [[Cell]] 

    print "Part 1"
    print $ part1 numbers boards
    print "Part 2"
    print $ part2 numbers boards (-1,[])

----------------------------------

part1 :: [Int] -> [[Cell]] -> Int
part1 [] _ = -1
part1 (i:numbers) boards = do
    let run = map (mark i) boards
    let checked = [(j,b) | j <- [0..((length run)-1)], let b = checkBoard $ run !! j]
    let b = [run !! j | (j,b) <- checked, b]
    part1' b i numbers run
        

part1' :: [[Cell]] -> Int -> [Int] -> [[Cell]] -> Int
part1' [] _ numbers run = part1 numbers run
part1' b  i _       _   = i * sumUnmarked (head b)

----------------------------------

part2 :: [Int] -> [[Cell]] -> (Int, [Cell]) -> Int
part2 []          _      (n,board) = n * sumUnmarked board
part2 (i:numbers) boards latest    = do
    let run = map (mark i) boards
    let checked = [(j,b) | j <- [0..((length run)-1)], let b = checkBoard $ run !! j]
    let winners = [run !! j | (j,b) <- checked, b]
    part2' winners i numbers (filter (\x -> x `notElem` winners) run) latest

part2' :: [[Cell]] -> Int -> [Int] -> [[Cell]] -> (Int, [Cell]) -> Int
part2' [] _ numbers run latest = part2 numbers run latest
part2' (b:_) i numbers run _   = part2 numbers run (i, b)

----------------------------------

split' :: [String] -> [[String]]
split' [] = []
split' b  = (foldr (++) [] $ [filter (/="") $ splitOn " " k | k <- take 5 b]):(split' $ drop 5 b)

-- Only 5 at a time
checkRow :: [Cell] -> Bool
checkRow ((Cell _ False):rest) = False
checkRow ((Cell _ True):rest)  = checkRow rest
checkRow []                    = True

-- Whole board (- index)
checkCol :: [Cell] -> Bool
checkCol ((Cell _ False):rest) = False
checkCol ((Cell _ True):rest)  = checkCol $ drop 4 rest
checkCol []                    = True

checkBoard :: [Cell] -> Bool
checkBoard b = or [(checkRow $ take 5 $ drop (i*5) b) || (checkCol $ drop i b) | i <- [0..4]]

check' :: Cell -> Bool
check' (Cell _ b) = not b

num :: Cell -> Int
num (Cell n _) = n

sumUnmarked :: [Cell] -> Int
sumUnmarked b = foldr (+) 0 $ map (num) $ filter (check') b

mark :: Int -> [Cell] -> [Cell]
mark _ [] = []
mark n ((Cell n' b):board)
    | n == n'   = (Cell n' True):(mark n board)
    | otherwise = (Cell n' b   ):(mark n board)