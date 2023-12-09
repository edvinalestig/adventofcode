import Data.Text (splitOn, pack, unpack)
import qualified Data.Map as M
import Data.Maybe (fromJust)

main :: IO ()
main = part2

part1 :: IO ()
part1 = do
    [instr, nodes'] <- map unpack . splitOn (pack "\n\n") . pack <$> readFile "input.txt"
    let instructions = cycle instr
        nodes = M.fromList $ map (\s -> (take 3 s, (take 3 $ drop 7 s, take 3 $ drop 12 s))) $ lines nodes'
    print $ findZZZ 0 instructions nodes "AAA"
    where
        findZZZ :: Integer -> String -> M.Map String (String,String) -> String -> Integer
        findZZZ n _      _     "ZZZ"   = n
        findZZZ n (i:is) nodes current = case i of
            'L' -> findZZZ (n+1) is nodes $ fst . fromJust $ M.lookup current nodes
            'R' -> findZZZ (n+1) is nodes $ snd . fromJust $ M.lookup current nodes

part2 :: IO () -- Runs out of memory..
part2 = do
    [instr, nodes'] <- map unpack . splitOn (pack "\n\n") . pack <$> readFile "input.txt"
    let instructions = cycle instr
        nodes = M.fromList $ map (\s -> (take 3 s, (take 3 $ drop 7 s, take 3 $ drop 12 s))) $ lines nodes'
        starts = filter (\s -> last s == 'A') $ M.keys nodes
    print $ findZs 0 instructions nodes starts

findZs :: Integer -> String -> M.Map String (String,String) -> [String] -> Integer
findZs n (i:is) nodes currents = if not (any (\s -> last s /= 'Z') currents) then n else case i of
    'L' -> findZs (n+1) is nodes $ map (\curr -> fst . fromJust $ M.lookup curr nodes) currents
    'R' -> findZs (n+1) is nodes $ map (\curr -> snd . fromJust $ M.lookup curr nodes) currents
