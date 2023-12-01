import Data.Char
import Data.Map (Map, keys)
import qualified Data.Map as Map
import qualified Data.Bifunctor

digits :: Map String Int
digits = Map.fromList [
    ("one", 1), ("two", 2), ("three", 3),
    ("four", 4), ("five", 5), ("six", 6),
    ("seven", 7), ("eight", 8), ("nine", 9)]

main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    let s = sum $ zipWith (\a b -> 10*a + b) (map firstdigit inputs) $ map lastdigit inputs
    print s

firstdigit :: String -> Int
firstdigit = flip firstdigit' digits

firstdigit' :: String -> Map String Int -> Int
firstdigit' [] _ = error "No first digit found"
firstdigit' str@(s:ss) digits
    | isDigit s = fromEnum s - 48
    | otherwise = do
        let matches = filter (>0) $
                map ((\a b -> Map.findWithDefault 0 (take (length b) a) digits) str) $
                    keys digits
        if not (null matches) then
            head matches
        else
            firstdigit' ss digits

lastdigit :: String -> Int
lastdigit s = firstdigit' (reverse s) $ Map.fromList $ map (Data.Bifunctor.first reverse) $ Map.toList digits
