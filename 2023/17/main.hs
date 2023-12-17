import Data.Char (digitToInt)
import qualified Data.Heap as H
import qualified Data.Map  as M
import Control.Monad.Trans.State (evalState, get, gets, modify, put, State)
import GHC.Base (maxInt)

type Coord     = (Int,Int)
data Direction = Lft | Rght | Up | Dwn deriving (Show, Eq, Ord)
type PosInfo   = (Coord, Direction, Int)
data Part      = Part1 | Part2 deriving (Eq)
type S         = State Environment

data Environment = Env {
    nodes    :: M.Map Coord Int,
    openSet  :: H.MinPrioHeap Int PosInfo,
    cameFrom :: M.Map PosInfo PosInfo,
    gScore   :: M.Map PosInfo Int,
    fScore   :: M.Map PosInfo Int,
    maxX     :: Int,
    maxY     :: Int
} deriving (Show)

part1 :: IO ()
part1 = run Part1

part2 :: IO ()
part2 = run Part2

run :: Part -> IO ()
run part = do
    inp <- map (map digitToInt) . lines <$> readFile "input.txt"
    let nodes = M.fromList $ concat [[((x,y),i) | (x,i) <- zip [0..] row] | (y,row) <- zip [0..] inp]
    let goal@(maxX,maxY) = (length (head inp) - 1, length inp - 1)
    let result = evalState (aStar' part (h goal) goal) (startEnv nodes maxX maxY) -- best path
    let heatLoss = sum (map (nodes M.!) result) - nodes M.! (0,0)
    print heatLoss

h :: Coord -> Coord -> Int -- Manhattan distance
h (cx,cy) (gx,gy) = abs (cx-gx) + abs (cy-gy)

startEnv :: M.Map Coord Int -> Int -> Int -> Environment
startEnv nodes maxX maxY = Env {
    nodes    = nodes,
    openSet  = H.fromList [(0, ((0,0), Rght, 0)), (0, ((0,0), Dwn, 0))],
    cameFrom = M.empty,
    gScore   = M.fromList [(((0,0), Rght, 0), 0), (((0,0), Dwn, 0), 0)],
    fScore   = M.empty,
    maxX     = maxX,
    maxY     = maxY
}

reconstructPath :: M.Map PosInfo PosInfo -> PosInfo -> [Coord]
reconstructPath _        ((0,0),_,_)         = [(0,0)]
reconstructPath cameFrom current@(coord,_,_) = do
    let nextCurrent = cameFrom M.! current
    reconstructPath cameFrom nextCurrent ++ [coord]

aStar' :: Part -> (Coord -> Int) -> Coord -> S [Coord]
aStar' part h goal = do
    openset <- gets openSet
    if H.isEmpty openset then
        error "Ran out of nodes to check"
    else do
        let Just ((prio,curr@(pos,dir,n)),openset') = H.view openset
        modify (\env -> env {openSet = openset'})

        if (part == Part1 && pos == goal) || (part == Part2 && pos == goal && n >=4) then do
            camefrom <- gets cameFrom
            return $ reconstructPath camefrom curr
        else do
            let neighbours = case part of
                    Part1 -> case dir of -- Max 3 before turning
                        Up   -> if n < 3 then [step pos Up   (n+1), step pos Lft 1, step pos Rght 1]
                                         else [step pos Lft 1, step pos Rght 1]
                        Dwn  -> if n < 3 then [step pos Dwn  (n+1), step pos Lft 1, step pos Rght 1]
                                         else [step pos Lft 1, step pos Rght 1]
                        Lft  -> if n < 3 then [step pos Lft  (n+1), step pos Up  1, step pos Dwn  1]
                                         else [step pos Up  1, step pos Dwn  1]
                        Rght -> if n < 3 then [step pos Rght (n+1), step pos Up  1, step pos Dwn  1]
                                         else [step pos Up  1, step pos Dwn  1]
                    Part2 -> case dir of -- At least 4 and max 10 before turning
                        Up   -> if      n < 4  then [step pos Up (n+1)]
                                else if n < 10 then [step pos Up (n+1), step pos Lft 1, step pos Rght 1]
                                               else [step pos Lft 1, step pos Rght 1]
                        Dwn  -> if      n < 4  then [step pos Dwn (n+1)]
                                else if n < 10 then [step pos Dwn (n+1), step pos Lft 1, step pos Rght 1]
                                               else [step pos Lft 1, step pos Rght 1]
                        Lft  -> if      n < 4  then [step pos Lft (n+1)]
                                else if n < 10 then [step pos Lft (n+1), step pos Up 1, step pos Dwn 1]
                                               else [step pos Up 1, step pos Dwn 1]
                        Rght -> if      n < 4  then [step pos Rght (n+1)]
                                else if n < 10 then [step pos Rght (n+1), step pos Up 1, step pos Dwn 1]
                                               else [step pos Up 1, step pos Dwn 1]

            mapM_ (calcNeighbour h curr) neighbours
            aStar' part h goal
    where
        calcNeighbour :: (Coord -> Int) -> PosInfo -> PosInfo -> S ()
        calcNeighbour h current next@(pos@(x,y),_,_) = do
            env <- get
            if x < 0 || x > maxX env || y < 0 || y > maxY env then return () else do
                let tentativeGScore = (gScore env ! current) + (nodes env ! pos)
                if tentativeGScore >= gScore env ! next then return () else do
                    let fscore      = tentativeGScore + h pos
                    let newCameFrom = M.insert next current (cameFrom env)
                    let newGScore   = M.insert next tentativeGScore (gScore env)
                    let newFScore   = M.insert next fscore (fScore env)
                    let newOpenSet  = H.insert (fscore, next) (openSet env)
                    put env {
                        cameFrom = newCameFrom,
                        gScore   = newGScore,
                        fScore   = newFScore,
                        openSet  = newOpenSet
                    }

(!) :: Ord k => M.Map k Int -> k -> Int
m ! key = M.findWithDefault maxInt key m -- Return "infinity" if not present in map

step :: Coord -> Direction -> Int -> PosInfo
step (x,y) Up   n = ((x,y-1), Up,   n)
step (x,y) Dwn  n = ((x,y+1), Dwn,  n)
step (x,y) Lft  n = ((x-1,y), Lft,  n)
step (x,y) Rght n = ((x+1,y), Rght, n)
