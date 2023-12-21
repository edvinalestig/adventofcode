{-# LANGUAGE LambdaCase #-}
import Data.Map.Strict (Map, (!?), (!))
import qualified Data.Map.Strict as M
import Data.List.Split (splitOn)
import Control.Monad.Trans.State.Strict (State, evalState, execState, gets, put)
import Data.Maybe (isNothing, fromJust)
import Control.Arrow ((***))

data Part = Part1 | Part2 deriving (Eq)

data Module 
    = FlipFlop Bool [String] Int Int
    | Conjunction (Map String Pulse) [String] Int Int
    | Broadcast [String] Int Int
    | Button Int
    deriving (Show)

data Pulse = LowPulse | HighPulse
    deriving (Show,Eq)

data Env = Env {
    queue :: [(String,String,Pulse)],
    states :: Map String Module
} deriving (Show)

main :: IO ()
main = part2

part1 :: IO ()
part1 = do
    inp <- lines <$> readFile "input.txt"
    let modules = M.fromList $ ("button", Button 0) : map makeModule inp
    let modules' = connectConjunctions modules
    let result = execState (run Part1 1000 M.empty) (emptyEnv modules')
    let total = countPulses result
    print total
    print $ uncurry (*) total

part2 :: IO ()
part2 = do
    inp <- lines <$> readFile "input.txt"
    let modules = M.fromList $ ("button", Button 0) : map makeModule inp
    let modules' = connectConjunctions modules
    let Conjunction m _ _ _ = modules' ! "gh" -- gh is the conjunction sending signals to rx
    let start = M.fromList [(k, (Nothing,Nothing)) | k <- M.keys m] -- All modules sending to gh
    let result = evalState (run Part2 0 start) (emptyEnv modules')
    let res = M.foldl' (\acc (n1,n2) -> n2-n1 : acc) [] result
    print $ foldl1 lcm res

emptyEnv :: Map String Module -> Env
emptyEnv st = Env {
    queue = [("button", "none", LowPulse)],
    states = st
}

makeModule :: String -> (String,Module)
makeModule s = do
    let [name,destinations] = splitOn " -> " s
    let dests = splitOn ", " destinations
    if name == "broadcaster" then
        (name, Broadcast dests 0 0)
    else if head name == '%' then
        (tail name, FlipFlop False dests 0 0)
    else
        (tail name, Conjunction M.empty dests 0 0)

connectConjunctions :: Map String Module -> Map String Module
connectConjunctions m = M.mapWithKey (f m) m
    where
        f :: Map String Module -> String -> Module -> Module
        f m name (Conjunction inputs dd lp hp) = Conjunction (M.foldlWithKey' (\acc key value -> 
            case value of
                Button              _   -> acc
                Conjunction _ dests _ _ -> if name `elem` dests then M.insert key LowPulse acc else acc
                Broadcast     dests _ _ -> if name `elem` dests then M.insert key LowPulse acc else acc
                FlipFlop _    dests _ _ -> if name `elem` dests then M.insert key LowPulse acc else acc
            ) M.empty m) dd lp hp
        f _ _ x = x

countPulses :: Env -> (Int,Int)
countPulses env = do
    let st = states env
    M.foldl' (\(l,h) -> \case
        Button lp -> (l+lp,h)
        Broadcast _ lp hp -> (l+lp, h+hp)
        Conjunction _ _ lp hp -> (l+lp, h+hp)
        FlipFlop _ _ lp hp -> (l+lp, h+hp)
        ) (0,0) st

run :: Part -> Int -> Map String (Maybe Int, Maybe Int) -> State Env (Map String (Int,Int))
run part i m = do
    q  <- gets queue
    st <- gets states
    if null q then do
        put $ Env {
            queue  = [("button", "none", LowPulse)],
            states = st
        }
        case part of
            Part1 -> if i <= 1 then return M.empty else run part (i-1) m
            Part2 -> if any (isNothing . snd) m then
                        run part (i+1) m
                    else
                        return $ M.map (fromJust *** fromJust) m
    else do
        let ((name, sender, pulse):qs) = q
        -- gh is the conjunction sending signals to rx
        let m' = if part == Part2 && name == "gh" && pulse == HighPulse then 
                    case m ! sender of
                        (Nothing, _) -> M.insert sender (Just i, Nothing) m
                        (Just x, Nothing) -> M.insert sender (Just x, Just i) m
                        _ -> m
                 else m

        case st !? name of
            Nothing -> put $ Env {
                queue = qs,
                states = st
            }
            Just (Button n) -> put $ Env {
                queue  = qs ++ [("broadcaster", "button", LowPulse)],
                states = M.insert "button" (Button (n+1)) st
            }
            Just (Broadcast dests lp hp) -> put $ Env {
                queue  = qs ++ [(d, name, pulse) | d <- dests],
                states = case pulse of
                    LowPulse  -> M.insert name (Broadcast dests (lp + length dests) hp) st
                    HighPulse -> M.insert name (Broadcast dests lp (hp + length dests)) st
            }
            Just (FlipFlop curr dests lp hp) -> case pulse of
                LowPulse -> if curr then
                        put $ Env {
                            queue  = qs ++ [(d, name, LowPulse) | d <- dests],
                            states = M.insert name (FlipFlop False dests (lp + length dests) hp) st
                        }
                    else
                        put $ Env {
                            queue  = qs ++ [(d, name, HighPulse) | d <- dests],
                            states = M.insert name (FlipFlop True dests lp (hp + length dests)) st
                        }
                HighPulse -> put $ Env {
                        queue = qs,
                        states = st
                    }
            Just (Conjunction prevPulses dests lp hp) -> do
                let newPulses = M.insert sender pulse prevPulses
                if all (==HighPulse) newPulses then
                    put $ Env {
                        queue  = qs ++ [(d, name, LowPulse) | d <- dests],
                        states = M.insert name (Conjunction newPulses dests (lp + length dests) hp) st
                    }
                else
                    put $ Env {
                        queue  = qs ++ [(d, name, HighPulse) | d <- dests],
                        states = M.insert name (Conjunction newPulses dests lp (hp + length dests)) st
                    }
        run part i m'
