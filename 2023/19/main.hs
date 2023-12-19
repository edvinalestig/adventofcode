{-# LANGUAGE LambdaCase #-}
import qualified Data.Map as M
import Data.Map (Map, (!))
import Data.List.Split (splitOn)
import Text.Regex (mkRegex, matchRegex)
import Data.Maybe (mapMaybe, fromJust, isNothing)
import Data.List (foldl', find)

data NextStep = A | R | Wf String | NextRule
    deriving (Show, Eq)
type RuleMap = Map String [(Char, Int -> Bool, NextStep)]

part1 :: IO ()
part1 = do
    [rules,parts'] <- map lines . splitOn "\n\n" <$> readFile "input.txt"
    let rx = mkRegex "(\\w+)\\{(.+)\\}"
    let ruleMatches = mapMaybe (matchRegex rx) rules
    let ruleMap = makeRuleMap ruleMatches
    let parts = map (map (read . drop 2) . splitOn "," . tail . init) parts' :: [[Int]]
    let res = map (run ruleMap) parts
    print $ sum res

run :: RuleMap -> [Int] -> Int
run rm xmas = case run' rm xmas "in" of
    A -> sum xmas
    R -> 0
    where
        run' :: RuleMap -> [Int] -> String -> NextStep
        run' rm xmas@[x,m,a,s] wf = do
            let rules = rm ! wf
            let (_,_,next) = fromJust $ find (\(c,f,n) -> case c of
                    'x' -> f x
                    'm' -> f m
                    'a' -> f a
                    's' -> f s
                    '-' -> f x) rules
            case next of
                Wf wf' -> run' rm xmas wf'
                a      -> a

makeRuleMap :: [[String]] -> RuleMap
makeRuleMap = foldl' makeRuleMap' M.empty
    where
        makeRuleMap' :: RuleMap -> [String] -> RuleMap
        makeRuleMap' m [name,conds] = do
            let conds' = map (splitOn ":") $ splitOn "," conds
            let rules  = map (\cond -> if length cond == 1 then ('-', const True, makeNext $ head cond)
                              else (head $ head cond, makeCond . tail $ head cond, makeNext $ last cond)) conds'
            M.insert name rules m

makeNext :: String -> NextStep
makeNext "A" = A
makeNext "R" = R
makeNext  a  = Wf a

makeCond :: String -> Int -> Bool
makeCond (op:n) a = case op of
    '>' -> a > read n
    '<' -> a < read n

------------------------------------------

type Range = (Int,Int)

part2 :: IO ()
part2 = do
    [rules,_] <- map lines . splitOn "\n\n" <$> readFile "input.txt"
    let rx = mkRegex "(\\w+)\\{(.+)\\}"
    let ruleMatches = mapMaybe (matchRegex rx) rules
    let ruleMap = M.fromList $ map (\[wf,conds] -> (wf, splitOn "," conds)) ruleMatches
    let ranges = applyWorkflow ruleMap "in" ((1,4000),(1,4000),(1,4000),(1,4000))
    print $ foldl' sumRanges 0 ranges

sumRanges :: Int -> (Range,Range,Range,Range) -> Int
sumRanges acc ((x1,x2), (m1,m2), (a1,a2), (s1,s2)) =
    acc + (1 + abs (x2-x1)) * (1 + abs (m2-m1)) * (1 + abs (a2-a1)) * (1 + abs (s2-s1))

applyWorkflow :: Map String [String] -> String -> (Range,Range,Range,Range) -> [(Range,Range,Range,Range)]
applyWorkflow ruleMap workflow ranges = do
    let nexts = foldl' applyRules [f ranges] $ ruleMap ! workflow
    concat (mapMaybe (ff ruleMap) nexts) -- Rejected ranges are removed by mapMaybe

f :: (a,b,c,d) -> (a,b,c,d,NextStep)
f (x,m,a,s) = (x,m,a,s,NextRule)

ff :: Map String [String] -> (Range,Range,Range,Range,NextStep) -> Maybe [(Range,Range,Range,Range)]
ff ruleMap = \case
    (x,m,a,s,A)     -> Just [(x,m,a,s)]
    (_,_,_,_,R)     -> Nothing
    (x,m,a,s,Wf wf) -> case applyWorkflow ruleMap wf (x,m,a,s) of
        [] -> Nothing
        z  -> Just z

applyRules :: [(Range,Range,Range,Range,NextStep)] -> String -> [(Range,Range,Range,Range,NextStep)]
applyRules acc rule = concatMap (\z@(x,m,a,s,next) -> if next == NextRule then applyRule rule (x,m,a,s) else [z]) acc

applyRule :: String -> (Range,Range,Range,Range) -> [(Range,Range,Range,Range,NextStep)]
applyRule rule (x,m,a,s) =
    if ':' `elem` rule then do
        let [c:op:n', next] = splitOn ":" rule
        let n = read n' :: Int
        case c of
            'x' -> do
                let (low,high) = splitRange op n x
                case op of
                    '<' ->  if isNothing low  then [(x, m, a, s, NextRule)] else
                            if isNothing high then [(fromJust low, m, a, s, makeNext next)] else
                            [(fromJust low, m, a, s, makeNext next), (fromJust high, m, a, s, NextRule)]
                    '>' ->  if isNothing high then [(x, m, a, s, NextRule)] else
                            if isNothing low  then [(fromJust high, m, a, s, makeNext next)] else
                            [(fromJust low, m, a, s, NextRule), (fromJust high, m, a, s, makeNext next)]
            'm' -> do
                let (low,high) = splitRange op n m
                case op of
                    '<' ->  if isNothing low  then [(x, m, a, s, NextRule)] else
                            if isNothing high then [(x, fromJust low, a, s, makeNext next)] else
                            [(x, fromJust low, a, s, makeNext next), (x, fromJust high, a, s, NextRule)]
                    '>' ->  if isNothing high then [(x, m, a, s, NextRule)] else
                            if isNothing low  then [(x, fromJust high, a, s, makeNext next)] else
                            [(x, fromJust low, a, s, NextRule), (x, fromJust high, a, s, makeNext next)]
            'a' -> do
                let (low,high) = splitRange op n a
                case op of
                    '<' ->  if isNothing low  then [(x, m, a, s, NextRule)] else
                            if isNothing high then [(x, m, fromJust low, s, makeNext next)] else
                            [(x, m, fromJust low, s, makeNext next), (x, m, fromJust high, s, NextRule)]
                    '>' ->  if isNothing high then [(x, m, a, s, NextRule)] else
                            if isNothing low  then [(x, m, fromJust high, s, makeNext next)] else
                            [(x, m, fromJust low, s, NextRule), (x, m, fromJust high, s, makeNext next)]
            's' -> do
                let (low,high) = splitRange op n s
                case op of
                    '<' ->  if isNothing low  then [(x, m, a, s, NextRule)] else
                            if isNothing high then [(x, m, a, fromJust low, makeNext next)] else
                            [(x, m, a, fromJust low, makeNext next), (x, m, a, fromJust high, NextRule)]
                    '>' ->  if isNothing high then [(x, m, a, s, NextRule)] else
                            if isNothing low  then [(x, m, a, fromJust high, makeNext next)] else
                            [(x, m, a, fromJust low, NextRule), (x, m, a, fromJust high, makeNext next)]
    else
        [(x, m, a, s, makeNext rule)] -- Base case

splitRange :: Char -> Int -> Range -> (Maybe Range, Maybe Range)
splitRange op n r@(low,high)
    | n < low  = (Nothing, Just r)
    | n > high = (Just r, Nothing)
    | n == low || n == high = undefined
    | otherwise = case op of
        '<' -> (Just (low, n-1), Just (n, high))
        '>' -> (Just (low, n), Just (n+1, high))

