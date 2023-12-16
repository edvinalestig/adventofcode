import qualified Data.Map as M
import Data.Map (Map, (!?), fromList)
import Control.Monad.Trans.State (execState, gets, modify, State)

--               type incoming light
data Tile = Tile Char [Direction]
    deriving (Show)
data Direction = Lft | Rght | Up | Dwn
    deriving (Show, Eq)

type Pos = (Int,Int,Direction)
type Tiles = State (Map (Int,Int) Tile)

main :: IO ()
main = do
    inp <- lines <$> readFile "input.txt"
    let tiles = fromList . concat $ [[((x,y), Tile c []) | (x,c) <- zip [0..] row] | (y,row) <- zip [0..] inp]
        maxX = length (head inp) - 1
        maxY = length inp - 1
        configs = [(0,i,Rght) | i <- [0..maxY]] ++ [(maxX,i,Lft) | i <- [0..maxY]] ++
                  [(i,0,Dwn)  | i <- [0..maxX]] ++ [(i,maxY,Up)  | i <- [0..maxX]]
        energized = map (run tiles) configs

    putStrLn $ "Part 1: " ++ show (head energized)
    putStrLn $ "Part 2: " ++ show (maximum energized)

run :: Map (Int,Int) Tile -> Pos -> Int
run tiles pos = do
    let result = execState (travel pos) tiles
    M.foldl (\acc (Tile _ dirs) -> if null dirs then acc else acc+1) 0 result

travel :: Pos -> Tiles ()
travel pos@(x,y,dir) = do
    tile <- gets (!? (x,y))
    case tile of
        Nothing -> return () -- Out of bounds
        Just (Tile c dirs) ->
            if dir `elem` dirs then
                return () -- Already visited, don't loop
            else do
                modify $ M.update (\_ -> Just $ Tile c (dir:dirs)) (x,y)
                case c of
                    '.' -> travel $ step pos
                    '-' -> if dir `elem` [Lft,Rght] then travel $ step pos else do
                            travel $ step (x,y,Lft) -- split
                            travel $ step (x,y,Rght)
                    '|' -> if dir `elem` [Up,Dwn] then travel $ step pos else do
                            travel $ step (x,y,Up) -- split
                            travel $ step (x,y,Dwn)
                    '/' -> case dir of
                        Lft  -> travel $ step (x,y,Dwn)
                        Dwn  -> travel $ step (x,y,Lft)
                        Rght -> travel $ step (x,y,Up)
                        Up   -> travel $ step (x,y,Rght)
                    '\\' -> case dir of
                        Lft  -> travel $ step (x,y,Up)
                        Up   -> travel $ step (x,y,Lft)
                        Dwn  -> travel $ step (x,y,Rght)
                        Rght -> travel $ step (x,y,Dwn)

step :: Pos -> Pos
step (x,y,dir) = case dir of
    Lft  -> (x-1, y, dir)
    Rght -> (x+1, y, dir)
    Up   -> (x, y-1, dir)
    Dwn  -> (x, y+1, dir)