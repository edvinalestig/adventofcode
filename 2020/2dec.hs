-- import System.IO
-- import Data.List

-- -- getMin :: String -> Int
-- -- getMin input = read mn input

-- -- mn :: String -> String
-- -- mn (c:rest)
-- --     | c == '-'  = ""
-- --     | otherwise = c : mn rest

-- -- getMax :: String -> Int
-- -- getMax input = read [input !! 2] :: Int

-- -- mx :: String -> String
-- -- mx (c:rest)
-- --     | c == ' '  = ""


-- getLetter :: String -> Char
-- getLetter input = input !! 4

-- getOccurences :: String -> Char -> Int
-- getOccurences input c = sum [1 | a <- input, a == c]

-- getPsw :: String -> String
-- getPsw (c:rest)
--     | c == ':'  = rest
--     | otherwise = getPsw rest

-- getMax input = elemIndex '-' input

-- main :: IO ()
-- main = do
--     handle <- openFile "2dec.txt" ReadMode
--     input <- hGetContents handle
--     let valid = length [1 | psw <- lines input, getOccurences (getPsw input) (getLetter psw) >= getMin psw, getOccurences (getPsw input) (getLetter psw) <= getMax psw]
--     print valid
--     hClose handle 
    