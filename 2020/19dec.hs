data Rule = Leaf Char | Rule [(Rule, Rule)] | Single [Rule] deriving Show
data PRule = PLeaf String Char | PRule String [(String, String)] | PSingle String [String] deriving Show

compilePRuleset :: [String] -> [PRule]
compilePRuleset [] = []
compilePRuleset (str:strs) = do
        let id = takeWhile (/=':') str
        let rules1 = chop (takeWhile (/='|') (dropWhile (/=' ') str))
        let rules2 = chop (dropWhile (/=' ') (dropWhile (/='|') str))
        case rules2 of
            [] -> compilePRule id [rules1] : compilePRuleset strs
            _  -> compilePRule id [rules1, rules2] : compilePRuleset strs


chop :: String -> String
chop s 
    | null s = []
    | head s == ' ' = (chop . tail) s
    | last s == ' ' = init s
    | otherwise     = s


splitParts :: [String] -> [[String]]
splitParts strs = [[chop $ takeWhile (/=' ') s, chop $ dropWhile (/=' ') s] | s <- strs]


compilePRule :: String -> [String] -> PRule
compilePRule id parts
    | null p2 = case p1 of
        ['\"':c:_,[]]   -> PLeaf id c
        [a,[]]          -> PSingle id [a]
        [a,b]           -> PRule id [(a,b)]
    | otherwise = case (p1,head p2) of
        ([a,[]],[b,[]]) -> PSingle id [a, b]
        ([a,b],[c,d])   -> PRule id [(a,b),(c, d)]
    where (p1:p2) = splitParts parts


getPRule :: [PRule] -> String -> PRule
getPRule (PLeaf n c:prules) id 
    | n == id   = PLeaf id c
    | otherwise = getPRule prules id
getPRule (PRule n a:prules) id 
    | n == id   = PRule id a
    | otherwise = getPRule prules id
getPRule (PSingle n a:prules) id
    | n == id   = PSingle id a
    | otherwise = getPRule prules id


compileRule :: [PRule] -> String -> Rule
compileRule prules id = do
    let prule = getPRule prules id
    case prule of
        (PLeaf _ c) -> Leaf c
        (PSingle _ pr) -> Single [compileRule prules p | p <- pr]
        (PRule _ pr) -> Rule [(compileRule prules p1, compileRule prules p2) | (p1,p2) <- pr]


checkRule :: String -> Rule -> (Bool,String) -- (success, matched chars)
checkRule (c:_) (Leaf ch) = (c == ch, [c])
-- a --
checkRule str (Single [a]) = checkRule str a
-- a or b --
checkRule str (Single [a,b]) = do 
    let (s1,m1) = checkRule str a
    if s1 then 
        (s1,m1)
    else
        checkRule str b
-- a and b --
checkRule str (Rule [(a,b)]) = do 
    let (s1,m1) = checkRule str a
    if not s1 then
        (s1,m1)
    else do
        let (s2,m2) = checkRule (drop (length m1) str) b
        (s1 && s2, m1 ++ m2)
-- (a and b) or (c and d)
checkRule str (Rule [ab,cd]) = do
    let (s1,m1) = checkRule str $ Rule [ab]
    if s1 then
        (s1,m1)
    else
        checkRule str $ Rule [cd]
checkRule _ _ = (False, "")


main :: IO ()
main = do
    input <- readFile "19dec.txt"
    let rules = lines $ map fst $ takeWhile (\(n, m) -> (n, m) /= ('\n', '\n')) $ zip input $ tail input
    let strs  = lines $ map fst $ dropWhile (\(n, m) -> (n, m) /= ('\n', '\n')) $ zip input $ tail input
    let prules = compilePRuleset rules

    part2 strs prules


part1 :: [String] -> [PRule] -> IO ()
part1 strs prules = do
    let rule = compileRule prules "0"
    print $ sum [1 | s <- strs, let (success,matched) = checkRule s rule, success && matched == s]


part2 :: [String] -> [PRule] -> IO ()
part2 strs prules = do
    let m = maximum $ map length strs
    let rule31 = compileRule prules "31"
    let rule42 = compileRule prules "42"
    let rule11 = compile11 m rule42 rule31 $ compileRule prules "11"
    let rule8  = compile8  m rule42 $ compileRule prules "8"
    -- let rule11 = compileRule prules "11"
    -- let rule8  = compileRule prules "8"

    -- Rule 0: 8 11
    let rule0 = Rule [(rule8, rule11)]

    -- print (sum [1 | s <- strs, let (success,matched) = checkRule2 m s [rule0, rule8, rule11, rule42, rule31], success && matched == s])
    print . sum $ [1 | s <- strs, let (success,matched) = checkRule s rule0, success && matched == s]


compile8 :: Int -> Rule -> Rule -> Rule
compile8 i rule42 rule8 
-- Rule 8: 42 | (42 8)
    | i <= 0    = rule8
    | otherwise = compile8 (i-1) rule42 (Single [rule42, Rule [(rule42, rule8)]])


compile11 :: Int -> Rule -> Rule -> Rule -> Rule
compile11 i rule42 rule31 rule11 
-- Rule 11: 42 31 | 42 (11 31)
    | i <= 0    = rule11
    | otherwise = compile11 (i-1) rule42 rule31 (Rule [(rule42, rule31), (rule42, Rule [(rule11, rule31)])])
