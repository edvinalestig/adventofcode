import Data.List (transpose)

part1 :: IO ()
part1 = run 2

part2 :: IO ()
part2 = run 1000000

run :: Int -> IO ()
run n = do
    inp <- lines <$> readFile "input.txt"
    -- Find rows/columns to expand
    let emptyRows    = [i | (row,i) <- zip inp             [0..], '#' `notElem` row]
        emptyColumns = [i | (row,i) <- zip (transpose inp) [0..], '#' `notElem` row]

    -- Find pairs
    let galaxies = concat [[(x,y) | (c,x) <- zip row [0..], c == '#'] | (row,y) <- zip inp [0..]]
        pairs    = [(g1,g2) | g1 <- galaxies, g2 <- galaxies] 
                -- includes both (a,b) and (b,a), halve distance later

    -- count distances
    let distance = foldl (\acc gx -> acc + calcDistance emptyRows emptyColumns gx) 0 pairs
    print $ distance `div` 2

    where
        calcDistance :: [Int] -> [Int] -> ((Int,Int),(Int,Int)) -> Int
        calcDistance rows cols ((x1,y1),(x2,y2)) = do
            let diffx   = abs (x1-x2)
                diffy   = abs (y1-y2)
                colplus = (n-1) * length (filter (\x -> (x<x1 && x>x2) || (x<x2 && x>x1)) cols)
                rowplus = (n-1) * length (filter (\y -> (y<y1 && y>y2) || (y<y2 && y>y1)) rows)
            diffx + diffy + colplus + rowplus
