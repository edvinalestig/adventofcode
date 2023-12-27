import Data.List.Split (splitOn)
import Data.List (intercalate)
import Control.Monad.Memo (for2, startEvalMemo, MonadMemo(..))

main :: IO ()
main = do
    inp <- map parse1 . lines <$> readFile "input.txt"
    let res1 = startEvalMemo $ mapM (uncurry countArrangementsM) inp
    putStrLn $ "Part 1: " ++ show (sum res1)

    inp <- map parse2 . lines <$> readFile "input.txt"
    let res2 = startEvalMemo $ mapM (uncurry countArrangementsM) inp
    putStrLn $ "Part 2: " ++ show (sum res2)
    where
        parse1 :: String -> (String,[Int])
        parse1 s = do
            let [str,ns] = words s
            (str, map read $ splitOn "," ns)
        parse2 :: String -> (String,[Int])
        parse2 s = do
            let [str,ns] = words s
            (intercalate "?" $ replicate 5 str, concat $ replicate 5 $ map read $ splitOn "," ns)

countArrangementsM :: MonadMemo (String, [Int]) Int m => String -> [Int] -> m Int
countArrangementsM "" [] = return 1
countArrangementsM "" _  = return 0
countArrangementsM s@(c:cs) ns = case c of
    '.' ->  for2 memo countArrangementsM cs ns
    '#' ->  if null ns then return 0 else do
                let (n:nums) = ns
                if length s == n && notElem '.' s && null nums then do
                    return 1
                else if length s > n && notElem '.' (take n s) && (s !! n) /= '#' then
                    for2 memo countArrangementsM (drop (1 + n) s) nums
                else return 0
    '?' ->  do
        n1 <- for2 memo countArrangementsM ('.':cs) ns
        n2 <- for2 memo countArrangementsM ('#':cs) ns
        return $ n1 + n2
