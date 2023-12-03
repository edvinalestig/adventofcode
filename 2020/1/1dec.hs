import System.IO

main = findTriple "1dec.txt"

findPair :: FilePath -> IO ()
findPair file = do

    withFile file ReadMode (\handle -> do  
        input <- hGetContents handle     
        let ints = [read n :: Integer | n <- words input]
        print [(i,j) | i <- ints, j <- ints, j + i == 2020 && (i > j)]
        )

findTriple :: FilePath -> IO ()
findTriple file = do
    input <- readFile "1dec.txt"
    -- let ints = [read n :: Integer | n <- words input]
    let ints = (map read . words) input
    print [(i,j,k) | i <- ints, j <- ints, k <- ints, j + i + k == 2020]
