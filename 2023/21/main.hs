import Data.HashMap.Strict (HashMap, (!?), (!))
import Data.Sequence (Seq, viewl, ViewL(..), (><))
import qualified Data.HashMap.Strict as M
import qualified Data.Sequence as S
import Control.Monad.Trans.State (State, execState, get, put)
import Data.List (foldl')

type Coord = (Int,Int)
data Env = Env {
    queue'   :: Seq (Coord, Int),
    visited' :: HashMap Coord Int,
    farm'    :: HashMap Coord Char
}

main :: IO ()
main = do
    inp <- lines <$> readFile "input.txt"
    let farm = M.fromList $ concat [[((x,y), c) | (x, c) <- zip [0..] row] | (y,row) <- zip [0..] inp]
    let start@(sx,sy) = fst $ head $ M.toList $ M.filter (=='S') farm
    let resState = execState bfs $ startEnv farm start
    let distances = map snd $ M.toList $ visited' resState
    let manhattanDists = map snd $ M.toList $ M.mapWithKey (\(x,y) _ -> abs (x-sx) + abs (y-sy)) $ visited' resState
    putStrLn $ "Part 1: " ++ show (length $ filter (\n -> even n && n <= 64) distances)

    -- Part 2
    let target = 26501365
    let n = target `div` length inp
    let border = target `rem` length inp
    let (odds, evens) = foldl' 
                        (\(ao,ae) x -> if even x then (ao, ae+1) else (ao+1, ae)) 
                        (0,0) distances
    let (odds', evens') = foldl'
                          (\(ao,ae) x -> if x > border then 
                                            if even x then(ao, ae+1) else (ao+1, ae)
                                         else (ao,ae))
                          (0,0) manhattanDists
    let result = (n+1)^2 * odds + n^2 * evens - (n+1) * odds' + n * evens'
    putStrLn $ "Part 2: " ++ show result

startEnv :: HashMap Coord Char -> Coord -> Env
startEnv farm start = Env {
    queue'   = S.singleton (start, 0),
    visited' = M.empty,
    farm'    = farm
}

bfs :: State Env ()
bfs = do
    env@Env {
        queue' = queue,
        visited' = visited,
        farm' = farm
    } <- get
    if S.null queue then return ()
    else do
        let ((current, n) :< q) = viewl queue
        if not $ M.member current visited then do
            let neighbours = filter (not . (`M.member` visited)) $ getNeighbours farm current
            put $ env {
                queue' = q >< S.fromList (map (, n+1) neighbours),
                visited' = M.insert current n visited
            }
            bfs
        else do
            put $ env {queue' = q}
            bfs

getNeighbours :: HashMap Coord Char -> Coord -> [Coord]
getNeighbours farm (x,y) =
    filter (\xy -> farm !? xy == Just '.' || farm !? xy == Just 'S') [(x,y-1),(x,y+1),(x+1,y),(x-1,y)]
