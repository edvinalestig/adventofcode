import Data.List

data Hand = Hand String Int
    deriving (Show)
data HandType = Junk | Pair | TwoPair | Three | House | Four | Five
    deriving (Show, Eq, Ord)

instance Eq Hand where
  (Hand c1 _) == (Hand c2 _) = c1 == c2

instance Ord Hand where
    h1@(Hand c1 _) <= h2@(Hand c2 _) = c1 == c2 || do
        let t1 = handType1 h1 -- Part 1
            t2 = handType1 h2
        -- let t1 = handType2 h1 -- Part 2
        --     t2 = handType2 h2
        if t1 == t2 then cmpCards c1 c2 else t1 < t2
        where
            cmpCards (c1:cs1) (c2:cs2) 
                | c1 == c2  = cmpCards cs1 cs2
                | otherwise = value c1 < value c2
            value 'A' = 14
            value 'K' = 13
            value 'Q' = 12
            value 'J' = 11 -- Part 1
            -- value 'J' = 1  -- Part 2
            value 'T' = 10
            value  n  = read [n]

handType1 :: Hand -> HandType
handType1 (Hand cards _) = do
    let groups = group $ sort cards
    case length groups of
        1 -> Five
        2 -> if sort (map length groups) == [1,4] then Four else House
        3 -> if sort (map length groups) == [1,1,3] then Three else TwoPair
        4 -> Pair
        5 -> Junk

handType2 :: Hand -> HandType
handType2 (Hand cards _) = do
    let groups = group $ sort $ cards \\ "JJJJJ"
    case length groups of
        0 -> Five
        1 -> Five
        2 -> if minimum (map length groups) == 2 then House else Four
        3 -> if sort (map length groups) == [1,2,2] then TwoPair else Three
        4 -> Pair
        5 -> Junk

main :: IO ()
main = do
    inp <- map words . lines <$> readFile "input.txt"
    let hands = sort $ map (\[c,b] -> Hand c (read b)) inp
    let total = foldl (\acc (Hand _ bid,i) -> acc + bid*i) 0 $ zip hands [1..]
    print total
