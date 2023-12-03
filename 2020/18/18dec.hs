import Data.Char

data Op = Add | Mul
data Expr = Val Int | Expr Expr Op Expr

main :: IO ()
main = do
    input <- readFile "18dec.txt"
    let strings = [filter (/= ' ') s | s <- lines input]
    (print . part1) strings

part1 :: [String] -> Int
part1 str = sum [(evalExpr . parseExpr) ex | ex <- str]

evalExpr :: Expr -> Int
evalExpr (Val n) = n
evalExpr (Expr e1 Add e2) = evalExpr e1 + evalExpr e2
evalExpr (Expr e1 Mul e2) = evalExpr e1 * evalExpr e2

-- HOW DO I PARSE??????
parseExpr :: String -> Expr
parseExpr (s:tring) 
    | isDigit s = Val (read [s])


parseExpr _ = Val 1
