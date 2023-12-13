{-# LANGUAGE LambdaCase #-}
import Data.List.Split (splitOn)
import Data.List (transpose)
import Data.Maybe (fromJust)

data Part = Part1 | Part2
    deriving (Eq)

part1 :: IO ()
part1 = run Part1

part2 :: IO ()
part2 = run Part2

run :: Part -> IO ()
run part = do
    inp <- map lines . splitOn "\n\n" <$> readFile "input.txt"
    print $ sum $ map (findReflection part) inp

findReflection :: Part -> [String] -> Int
findReflection part pattern = case findAboveReflection part pattern of
    Nothing -> fromJust $ findAboveReflection part $ transpose pattern
    Just n  -> 100 * n

findAboveReflection :: Part -> [String] -> Maybe Int
findAboveReflection part pattern = case upwards part pattern of
    Nothing -> do
        n <- upwards part (reverse pattern)
        Just $ length pattern - n
    n -> n

upwards :: Part -> [String] -> Maybe Int
upwards part pattern
    | length pattern == 1 = Nothing
    | otherwise = case checkPattern part pattern of
        Nothing -> upwards part $ init pattern
        n -> n
        where
            checkPattern = \case
                Part1 -> checkPattern1
                Part2 -> checkPattern2 0

checkPattern1 :: [String] -> Maybe Int
checkPattern1 ptn
    | null ptn = Just 0
    | head ptn == last ptn = do
        r <- checkPattern1 (tail $ init ptn)
        Just $ r + 1
    | otherwise = Nothing

checkPattern2 :: Int -> [String] -> Maybe Int
checkPattern2 n ptn
    | null ptn = if n == 1 then Just 0 else Nothing
    | odd (length ptn) = Nothing
    | head ptn == last ptn = do
        r <- checkPattern2 n (tail $ init ptn)
        Just $ r + 1
    | head ptn `diffBy1` last ptn = do
        r <- checkPattern2 (n+1) (tail $ init ptn)
        Just $ r + 1
    | otherwise = Nothing

diffBy1 :: String -> String -> Bool
diffBy1 s1 s2 = diffBy1' s1 s2 == 1
    where
        diffBy1' :: String -> String -> Int
        diffBy1' "" "" = 0
        diffBy1' (c1:s1) (c2:s2)
            | c1 == c2  = diffBy1' s1 s2
            | otherwise = diffBy1' s1 s2 + 1