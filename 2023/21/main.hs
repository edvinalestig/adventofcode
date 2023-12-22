import Data.Map (Map, (!?), (!))
import qualified Data.Map as M
import Control.Monad.Trans.State (State, evalState, get, put)
import Data.Maybe (isNothing, mapMaybe)

type Coord = (Int,Int)
data Env = Env {
    queue   :: [(Coord,Coord)],
    visited :: Map Coord Coord,
    plane   :: Map Coord Char
}

main :: IO ()
main = part1

part1 :: IO ()
part1 = do
    inp <- lines <$> readFile "input.txt"
    let farm = M.fromList $ concat [[((x,y), c) | (x, c) <- zip [0..] row] | (y,row) <- zip [0..] inp]
    let plots = M.foldlWithKey' (\acc key c -> if c == '#' then acc else key:acc) [] farm
    let res = mapMaybe (evalState bfs . startEnv farm) plots
    -- Takes a few minutes, runs quicker when compiled
    print $ length $ filter (\n -> even n && n <= 64) $ map length res

startEnv :: Map Coord Char -> Coord -> Env
startEnv farm start = Env {
    queue   = [((-1,-1),start)],
    visited = M.empty,
    plane   = farm
}

bfs :: State Env (Maybe [Coord])
bfs = do
    env <- get
    if null (queue env) then
        -- Start unreachable
        return Nothing
    else do
        let (prev,current) = head $ queue env
        if isNothing (visited env !? current) then do
            let farm = plane env
            if farm ! current == 'S' then
                return $ Just $ backtrace (visited env) prev
            else do
                let neighbours = getNeighbours farm current
                put $ env {
                    queue = tail (queue env) ++ map (current,) neighbours,
                    visited = M.insert current prev $ visited env
                }
                bfs
        else do
            put $ env {
                queue = tail (queue env)
            }
            bfs

backtrace :: Map Coord Coord -> Coord -> [Coord]
backtrace visited current =
    case visited !? current of
        Just next -> current : backtrace visited next
        Nothing   -> []

getNeighbours :: Map Coord Char -> Coord -> [Coord]
getNeighbours farm (x,y) =
    filter (\xy -> farm !? xy == Just '.' || farm !? xy == Just 'S') [(x,y-1),(x,y+1),(x+1,y),(x-1,y)]
