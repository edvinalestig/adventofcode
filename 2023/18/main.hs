{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

import Text.Regex (mkRegex, matchRegex)
import Data.Maybe (mapMaybe, fromJust)
import Data.List (foldl', find)
import Data.List.Split (chunksOf)
import Control.Monad (when)
import Numeric (readHex)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

data Part = Part1 | Part2
data Direction = U | D | L | R deriving (Show, Eq)
type Coord = (Int, Int)
type Range = ((Int, Int), Direction)

run :: Part -> IO ()
run part = do
    inp <- lines <$> readFile "input.txt"
    let rx = mkRegex "([LDRU]+) ([0-9]+) \\(#([0-9a-f]+)\\)"
    let input = map (\[d,n,hx] -> (d, read n, hx)) $ mapMaybe (matchRegex rx) inp :: [(String, Int, String)]
    when (length inp /= length input) $ error "Something didn't parse.."

    case part of
        Part1 -> part1 input
        Part2 -> part2 input

part1 :: [(String, Int, String)] -> IO ()
part1 input = do
    let (_,vertM,horM) = foldl' makeMaps ((0,0), IM.empty, IM.empty) input
    let minY = minimum $ IM.keys horM
    let maxY = maximum $ IM.keys horM
    let rows = [digRow vertM horM y | y <- [minY..maxY]]
    print $ sum rows

makeMaps :: (Coord, IntMap [Range], IntMap [Range]) -> (String, Int, a) -> (Coord, IntMap [Range], IntMap [Range])
makeMaps ((cx,cy), vertM, horM) (d,n,_) = case d of
    "R" -> ((cx+n, cy),
            vertM,
            IM.insertWith (++) cy [((cx, cx+n), R)] horM)
    "L" -> ((cx-n, cy),
            vertM,
            IM.insertWith (++) cy [((cx-n, cx), L)] horM)
    "U" -> ((cx, cy-n),
            IM.insertWith (++) cx [((cy-n, cy), U)] vertM,
            horM)
    "D" -> ((cx, cy+n),
            IM.insertWith (++) cx [((cy, cy+n), D)] vertM,
            horM)

part2 :: [(String, Int, String)] -> IO ()
part2 input = do
    let (_,vertM,horM) = foldl' makeMaps2 ((0,0), IM.empty, IM.empty) input
    let minY = minimum $ IM.keys horM
    let maxY = maximum $ IM.keys horM
    let rows = [digRow vertM horM y | y <- [minY..maxY]]
    print $ sum rows

makeMaps2 :: (Coord, IntMap [Range], IntMap [Range]) -> (a, b, String) -> (Coord, IntMap [Range], IntMap [Range])
makeMaps2 ((cx,cy), vertM, horM) (_,_,hx) = do
    let n = fst $ head $ readHex $ init hx
    case last hx of
        '0' -> ((cx+n, cy),
                vertM,
                IM.insertWith (++) cy [((cx, cx+n), R)] horM)
        '1' -> ((cx, cy+n),
                IM.insertWith (++) cx [((cy, cy+n), D)] vertM,
                horM)
        '2' -> ((cx-n, cy),
                vertM,
                IM.insertWith (++) cy [((cx-n, cx), L)] horM)
        '3' -> ((cx, cy-n),
                IM.insertWith (++) cx [((cy-n, cy), U)] vertM,
                horM)

digRow :: IntMap [Range] -> IntMap [Range] -> Int -> Int
digRow vertM horM y = do
        let horRanges = IM.findWithDefault [] y horM
        let vertWalls = IM.foldlWithKey' -- Slow but works
                (\acc key ranges -> acc ++
                    map (\(_,d) -> (key,d))
                        (filter (\((start,end),_) -> start <= y && end >= y) ranges))
                        [] vertM
        if null horRanges then do
            let pairs = chunksOf 2 vertWalls
            sum $ map (\[a,b] -> 1 + abs (fst a - fst b)) pairs
        else
            sumRow vertWalls horRanges y 0 False 0
        where
            sumRow :: [(Int, Direction)] -> [Range] -> Int -> Int -> Bool -> Int -> Int
            sumRow []             _         _ _     _     acc = acc
            sumRow (vw:vertWalls) horRanges y prevX False acc =
                -- Not inside the lagoon
                if vw `elem'` horRanges then do
                    let rg = fst $ find' vw horRanges
                    if fst rg == fst vw then do
                        -- Start of trench
                        let dir1 = snd vw
                        let dir2 = snd $ head vertWalls
                        if dir1 /= dir2 then
                            -- U-turn, not inside, just count the walls
                            sumRow (tail vertWalls) horRanges y prevX False (acc + 1 + abs (uncurry (-) rg))
                        else
                            sumRow (tail vertWalls) horRanges y (fst rg) True acc
                    else
                        -- End of trench, should never be reached
                        error "Reached end of trench"
                else
                    sumRow vertWalls horRanges y (fst vw) True acc
            sumRow (vw:vertWalls) horRanges y prevX True acc =
                -- Inside the lagoon
                if vw `elem'` horRanges then do
                    let rg = fst $ find' vw horRanges
                    if fst rg == fst vw then do
                        -- Start of trench
                        let dir1 = snd vw
                        let dir2 = snd $ head vertWalls
                        if dir1 /= dir2 then
                            -- U-turn, inside, count it all
                            sumRow (tail vertWalls) horRanges y prevX True acc
                        else
                            sumRow (tail vertWalls) horRanges y (snd rg) False (acc + 1 + abs (snd rg - prevX))
                    else
                        -- End of trench, should never be reached
                        error "Reached end of trench"
                else
                    sumRow vertWalls horRanges y (fst vw) False (acc + 1 + abs (fst vw - prevX))

            elem' :: (Int, Direction) -> [Range] -> Bool
            elem' (n,_) = any (\((a,b),_) -> n == a || n == b)

            find' :: (Int, Direction) -> [Range] -> Range
            find' (n,_) = fromJust . find (\((a,b),_) -> n == a || n == b)
