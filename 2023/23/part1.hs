import Data.Map (Map, (!?), (!))
import qualified Data.Map as M
import Control.Monad.Trans.State -- (State, evalState, get, put)
import Data.Maybe (catMaybes, isJust)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.List (inits)

data Part = Part1 | Part2
type Coord = (Int,Int)
data Env = Env {
    queue   :: [(Coord, Coord, String, Int)], -- prev, next, id, len
    visited :: Map Coord (String, Int), -- string is the path identifier, int is the path length
    trails  :: Map Coord Char,
    target  :: Coord,
    part    :: Part
}

main :: IO ()
main = do
    inp <- lines <$> readFile "testinput.txt"
    let trailMap = M.fromList $ concat [[((x,y), c) | (x, c) <- zip [0..] row] | (y,row) <- zip [0..] inp]
    let target = (length (head inp) - 2, length inp - 1)
    p1 <- evalStateT bfs (startEnv trailMap (1,0) target Part1)
    print p1
    p2 <- evalStateT bfs (startEnv trailMap (1,0) target Part2)
    print p2
    -- 5102 too low

startEnv :: Map Coord Char -> Coord -> Coord -> Part -> Env
startEnv trailMap start tgt pt = Env {
    queue   = [((-1,-1), start, "1", 0)],
    visited = M.empty,
    trails  = trailMap,
    target  = tgt,
    part    = pt
}

bfs :: StateT Env IO Int
bfs = do
    env <- get
    if null (queue env) then do
        let (_,n) = visited env ! target env
        return n
    else do
        let (prev, current, ident, len) = head $ queue env
        let doNothing = do
                put $ env {
                    queue = tail (queue env)
                }
                bfs
        let doNext = do
                let nbf = case part env of
                        Part1 -> getNeighbours
                        Part2 -> getNeighbours2
                let neighbours = nbf (trails env) prev current
                let idents = if length neighbours > 1 
                             then map (\i -> ident ++ show i) [1..length neighbours] 
                             else [ident] -- extend path ids. 2 nb: "12" -> "121", "122"
                             -- Earlier steps are always the beginning of the id
                put $ env {
                    queue = tail (queue env) ++ zipWith (\n i -> (current, n, i, len+1)) neighbours idents,
                    visited = M.insert current (ident, len) $ visited env
                }
                bfs

        case visited env !? current of
            Nothing -> doNext
            Just (ident', l) ->
                if l < len && ident' `notElem` inits ident
                then doNext
                else doNothing

getNeighbours :: Map Coord Char -> Coord -> Coord -> [Coord]
getNeighbours trailMap prev c@(x,y) =
    filter (/=prev) $ case trailMap ! c of
        '>' -> [(x+1,y)]
        '<' -> [(x-1,y)]
        'v' -> [(x,y+1)]
        '^' -> [(x,y-1)]
        '.' -> catMaybes [
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
