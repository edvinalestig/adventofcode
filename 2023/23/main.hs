import Data.Map (Map, (!?), (!))
import qualified Data.Map as M
import qualified Data.HashSet as HS
import Control.Monad.Trans.State (State, evalState, get, gets, put)
import Data.Maybe (catMaybes, isJust)
import Data.List (foldl')
import Control.DeepSeq (deepseq)
import GHC.Base (minInt)

data Part = Part1 | Part2
type Coord = (Int,Int)
data Env = Env {
    queue   :: [(Coord, Coord, Coord, Int)],
    visited :: [(Coord, Coord, Int)],
    trails  :: Map Coord Char,
    target  :: Coord,
    part    :: Part
}

main :: IO ()
main = do
    inp <- lines <$> readFile "input.txt"
    let trailMap = M.fromList $ concat [[((x, y), c) | (x, c) <- zip [0..] row] | (y, row) <- zip [0..] inp]
    let start = (1, 0)
    let target = (length (head inp) - 2, length inp - 1)

    let edges1 = evalState dfs (startEnv trailMap start target Part1)
    let graph1 = foldl' (\m (c1, c2, n) -> M.insertWith (++) c1 [(c2, n)] m) M.empty edges1
    let n1     = startSearch graph1 target start
    putStrLn $ "Part 1: " ++ show n1

    let edges2 = evalState dfs (startEnv trailMap start target Part2)
    let graph2 = foldl' (\m (c1, c2, n) -> M.insertWith (++) c1 [(c2, n)] m) M.empty edges2
    let n2     = startSearch graph2 target start
    putStrLn $ "Part 2: " ++ show n2


giveNext :: Map Coord [(Coord, Int)] -> Coord -> Coord -> [(Coord, Int)]
giveNext graph current prev = do
    filter ((/=prev) . fst) $ M.findWithDefault [] current graph

startEnv :: Map Coord Char -> Coord -> Coord -> Part -> Env
startEnv trailMap start tgt pt = Env {
    queue   = [(start, start, start, 0)],
    visited = [],
    trails  = trailMap,
    target  = tgt,
    part    = pt
}

-- Create the graph of paths between intersections
dfs :: State Env [(Coord, Coord, Int)]
dfs = do
    env <- get
    if null (queue env) then return $ visited env
    else do
        let (origin, prev, current, len) = head $ queue env
        if isIntersection (trails env) current || current == target env then
            if (origin, current, len) `elem` visited env then do
                put $ env {
                    queue = tail (queue env)
                }
                dfs
            else do
                let nbf = case part env of
                        Part1 -> getNeighbours
                        Part2 -> getNeighbours2
                let neighbours = nbf (trails env) prev current
                put $ env {
                    queue = map (current, current, , 1) neighbours ++ tail (queue env),
                    visited = (origin, current, len) : visited env
                }
                dfs
        else do
            let nbf = case part env of
                    Part1 -> getNeighbours
                    Part2 -> getNeighbours2
            let neighbours = nbf (trails env) prev current
            put $ env {
                queue = map (origin, current, , len+1) neighbours ++ tail (queue env)
            }
            dfs


getNeighbours :: Map Coord Char -> Coord -> Coord -> [Coord]
getNeighbours trailMap prev c@(x,y) =
    filter (/=prev) $ case trailMap !? c of
        Nothing  -> []
        Just '>' -> [(x+1,y)]
        Just '<' -> [(x-1,y)]
        Just 'v' -> [(x,y+1)]
        Just '^' -> [(x,y-1)]
        Just '.' -> catMaybes [
            case trailMap !? (x,y-1) of
                Just '.' -> Just (x,y-1)
                Just '^' -> Just (x,y-1)
                _        -> Nothing,
            case trailMap !? (x,y+1) of
                Just '.' -> Just (x,y+1)
                Just 'v' -> Just (x,y+1)
                _        -> Nothing,
            case trailMap !? (x-1,y) of
                Just '.' -> Just (x-1,y)
                Just '<' -> Just (x-1,y)
                _        -> Nothing,
            case trailMap !? (x+1,y) of
                Just '.' -> Just (x+1,y)
                Just '>' -> Just (x+1,y)
                _        -> Nothing
            ]

getNeighbours2 :: Map Coord Char -> Coord -> Coord -> [Coord]
getNeighbours2 trailMap prev (x,y) =
    filter
        (\xy -> isJust (trailMap !? xy) && trailMap !? xy /= Just '#' && xy /= prev)
        [(x,y-1),(x,y+1),(x+1,y),(x-1,y)]

isIntersection :: Map Coord Char -> Coord -> Bool
isIntersection trailMap (x,y) = all (\xy -> trailMap !? xy /= Just '.') 
                                    [(x,y-1),(x,y+1),(x+1,y),(x-1,y)]

--------

data DFSEnv = DFSEnv {
    graph' :: Map Coord [(Coord, Int)],
    target' :: Coord,
    visited' :: HS.HashSet Coord
}

startSearch :: Map Coord [(Coord, Int)] -> Coord -> Coord -> Int
startSearch graph target start = evalState (search start) $ DFSEnv {
    graph'   = graph,
    target'  = target,
    visited' = HS.empty
}

search :: Coord -> State DFSEnv Int
search node = do
    env@DFSEnv {
        graph'   = graph,
        target'  = target,
        visited' = visited
    } <- get
    if node == target then return 0 else do
        put $ env {visited' = HS.insert node visited}

        let neighbours = filter ((`notElem` visited) . fst) $ graph ! node
        m <- mapM (\(next, n) -> search next >>= (\i -> return $ i+n)) neighbours
        let mx = maximum (minInt:m)

        newVisited <- gets visited'
        put $ env {visited' = HS.delete node newVisited}
        mx `deepseq` return mx
