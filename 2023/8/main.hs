import Data.List.Split (splitOn)
import Data.Map ((!), Map, fromList, keys)

part1 :: IO ()
part1 = do
    [instr, nodes'] <- splitOn "\n\n" <$> readFile "input.txt"
    let instructions = cycle instr
        nodes = fromList $ map (\s -> (take 3 s, (take 3 $ drop 7 s, take 3 $ drop 12 s))) $ lines nodes'
    print $ findZZZ 0 instructions nodes "AAA"
    where
        findZZZ :: Integer -> String -> Map String (String,String) -> String -> Integer
        findZZZ n _      _     "ZZZ"   = n
        findZZZ n (i:is) nodes current = case i of
            'L' -> findZZZ (n+1) is nodes $ fst $ nodes ! current
            'R' -> findZZZ (n+1) is nodes $ snd $ nodes ! current

-- Ugly solution and isn't really general. Does not take the instruction length into account
-- and assumes it only reaches one state ending with Z.
part2 :: IO ()
part2 = do
    [instr, nodes'] <- splitOn "\n\n" <$> readFile "input.txt"
    let instructions = cycle instr
        nodes = fromList $ map (\s -> (take 3 s, (take 3 $ drop 7 s, take 3 $ drop 12 s))) $ lines nodes'
        starts = filter (\s -> last s == 'A') $ keys nodes
        loopLengths = map (findZ 0 instructions nodes []) starts
    print $ foldl1 lcm loopLengths

findZ :: Integer -> String -> Map String (String,String) -> [(Integer,String)] -> String -> Integer
findZ n (i:is) nodes visited current = do
    let prevZs = filter (\(n',node) -> current == node) visited
    if not $ null prevZs then
        n - fst (head prevZs)
    else do
        let visited' = if last current == 'Z' then (n, current):visited else visited
        case i of
            'L' -> findZ (n+1) is nodes visited' $ fst $ nodes ! current
            'R' -> findZ (n+1) is nodes visited' $ snd $ nodes ! current
