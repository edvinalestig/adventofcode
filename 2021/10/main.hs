{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Data.List (sort)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = lines input

    print "Part 1"
    print $ part1 instructions
    print "Part 2"
    print $ part2 $ filter isNotCorrupted instructions

part1 :: [String] -> Int
part1 input = sum $ map (\x -> score $ f x []) input

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score _   = 0

isNotCorrupted :: String -> Bool
isNotCorrupted string = f string [] == '0'

f :: String -> [Char] -> Char
f []            _  = '0' -- Not corrupted
f (char:string) [] = f string [char]
f (char:string) (top:stack) 
    | char `elem` ['(','[','{','<'] = f string (char:top:stack)
    | ct == (')','(') || ct == (']','[') || ct == ('}','{') || ct == ('>','<') = f string stack
    | otherwise = char
        where ct = (char,top)


----------------------

part2 :: [String] -> Int
part2 input = middle . sort $ map (\x -> calc $ stack' x []) input

stack' :: String -> [Char] -> [Char]
stack' []            stack       = stack
stack' (char:string) []          = stack' string [char]
stack' (char:string) (top:stack) 
    | char `elem` ['(','[','{','<'] = stack' string (char:top:stack)
    | ct == (')','(') || ct == (']','[') || ct == ('}','{') || ct == ('>','<') = stack' string stack
    | otherwise = error "Corrupted string"
        where ct = (char,top)

score' :: Char -> Int
score' '(' = 1
score' '[' = 2
score' '{' = 3
score' '<' = 4

calc :: [Char] -> Int
calc = foldl (\x y -> x*5 + score' y) 0

middle :: [a] -> a
middle list = list !! (length list `div` 2)
