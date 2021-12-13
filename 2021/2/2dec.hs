import Data.List.Split

part1 :: [(String, Int)] -> Int
part1 ins = run ins 0 0

run :: [(String, Int)] -> Int -> Int -> Int
run (("forward", n):rest) vert hori = run rest vert (hori + n)
run (("up"     , n):rest) vert hori = run rest (vert - n) hori
run (("down"   , n):rest) vert hori = run rest (vert + n) hori
run _                     vert hori = vert * hori

part2 :: [(String, Int)] -> Int
part2 ins = run2 ins 0 0 0

run2 :: [(String, Int)] -> Int -> Int -> Int -> Int
run2 (("forward", n):rest) vert hori aim = run2 rest (vert + n * aim) (hori + n) aim 
run2 (("up"     , n):rest) vert hori aim = run2 rest vert hori (aim - n)
run2 (("down"   , n):rest) vert hori aim = run2 rest vert hori (aim + n)
run2 _                     vert hori _   = vert * hori


main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = [(dir, read steps) | k <- lines input, let (dir:steps:_) = splitOn " " k] 
    print "Part 1"
    print $ part1 instructions
    print "Part 2"
    print $ part2 instructions