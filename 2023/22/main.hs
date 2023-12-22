{-# LANGUAGE LambdaCase #-}
import qualified Data.IntMap as IM
import qualified Data.Map    as M
import qualified Data.Heap   as H
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.List.Split (splitOn)
import Data.List (sort, nub, foldl')
import Data.Maybe (mapMaybe)

data Brick = Brick (Int, Int, Int) Int Int Int -- x, y, z lengths
    deriving (Show, Eq)
type PQ a = H.MinPrioHeap Int a
type BrickMap = IntMap (Map (Int, Int) Brick) -- Each z value has a map with (x,y) coords and its corresponding brick

instance Ord Brick where
    (<=) :: Brick -> Brick -> Bool
    Brick (_,_,z1) _ _ _ <= Brick (_,_,z2) _ _ _ = z1 <= z2


main :: IO ()
main = do
    inp <- map (map (map read . splitOn ",") . splitOn "~") . lines <$> readFile "input.txt" :: IO [[[Int]]]
    let bricks = sort $ map makeBrick inp -- sorts lowest to highest position
    let (settledBricksMap, settledBricks) = settleBricks bricks

    let disintegrable = filter (canDisintegrate settledBricksMap) settledBricks
    putStrLn $ "Part 1: " ++ show (length disintegrable)

    let chained = chainDisintegration settledBricksMap (reverse settledBricks) 0
    putStrLn $ "Part 2: " ++ show chained

makeBrick :: [[Int]] -> Brick
makeBrick [[x1,y1,z1],[x2,y2,z2]] = Brick (x1,y1,z1) (x2-x1) (y2-y1) (z2-z1)

settleBricks :: [Brick] -> (BrickMap, [Brick])
settleBricks = settleBricks' (IM.empty, [])
    where
        settleBricks' :: (BrickMap, [Brick]) -> [Brick] -> (BrickMap, [Brick])
        settleBricks' mb [] = mb
        settleBricks' (m,bs) (Brick (x,y,z) xx yy zz:bricks) = do
            let footprint = [(x',y') | x' <- [x..x+xx], y' <- [y..y+yy]]
                -- Find the highest occupied z then add 1
                newZ = 1 + IM.foldlWithKey (\acc z' brickmap ->
                                            if any (`M.member` brickmap) footprint then z' else acc) 0 m
                newBrick = Brick (x,y,newZ) xx yy zz
                offsets = [(x',y',z') | x' <- [0..xx], y' <- [0..yy], z' <- [0..zz]]
                newM = foldl' (\acc (x',y',z') ->
                               IM.alter (updateInnerMap (x+x',y+y') newBrick) (newZ+z') acc) m offsets
            settleBricks' (newM, newBrick : bs) bricks

        updateInnerMap :: Ord a => a -> b -> Maybe (Map a b) -> Maybe (Map a b)
        updateInnerMap coord brick = \case
            Nothing -> Just $ M.singleton coord brick
            Just m  -> Just $ M.insert coord brick m

canDisintegrate :: BrickMap -> Brick -> Bool
canDisintegrate m brick@(Brick (x,y,z) xx yy zz) = do
    let restingBricks = getRestingBricks m brick
    null restingBricks || all (any (/=brick) . restsOn m) restingBricks

restsOn :: BrickMap -> Brick -> [Brick]
restsOn m brick@(Brick (x,y,z) xx yy zz) =
    case m IM.!? (z-1) of
        Nothing -> []
        Just innerMap -> do
            let footprint = [(x',y') | x' <- [x..x+xx], y' <- [y..y+yy]]
            nub $ mapMaybe (innerMap M.!?) footprint

getRestingBricks :: BrickMap -> Brick -> [Brick]
getRestingBricks m brick@(Brick (x,y,z) xx yy zz) = do
    case m IM.!? (z+zz+1) of
        Nothing -> []
        Just innerMap -> do
            let footprint = [(x',y') | x' <- [x..x+xx], y' <- [y..y+yy]]
            nub $ mapMaybe (innerMap M.!?) footprint

chainDisintegration :: BrickMap -> [Brick] -> Int -> Int
chainDisintegration _ []             n = n
chainDisintegration m (brick:bricks) n = do
    let fallen = chainDisintegration' m [] 0 (H.singleton (0,brick))
    chainDisintegration m bricks $ n + fallen - 1 -- Do not include itself
    where
        -- A PQ is used to ensure all bricks below have been disintegrated
        chainDisintegration' :: BrickMap -> [Brick] -> Int -> PQ Brick -> Int
        chainDisintegration' m disintegrated n brickpq =
            case H.view brickpq of
                Nothing -> n
                Just ((_,brick), pq) -> do
                    let restingOn = restsOn m brick
                    if null disintegrated || all (`elem` disintegrated) restingOn then do
                        let restingBricks = getRestingBricks m brick
                        if null restingBricks then
                            chainDisintegration' m (brick:disintegrated) (n+1) pq
                        else
                            chainDisintegration' m (brick:disintegrated) (n+1) $
                                foldl' (\acc b@(Brick (_,_,z) _ _ _) -> 
                                    if b `notElem` acc then H.insert (z,b) acc else acc) 
                                    pq restingBricks
                    else do
                        chainDisintegration' m disintegrated n pq
