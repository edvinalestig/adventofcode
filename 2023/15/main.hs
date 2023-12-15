import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Map.Ordered as OM

part1 :: IO ()
part1 = do
    inp <- splitOn "," <$> readFile "input.txt"
    print $ sum $ map hash inp

hash :: String -> Int
hash = foldl (\acc c -> if c == '\n' then acc else ((acc + fromEnum c) * 17) `rem` 256) 0

part2 :: IO ()
part2 = do
    inp <- splitOn "," <$> readFile "input.txt"
    let mainmap = M.fromList $ zip [0..255] $ repeat OM.empty
    let focusPowers = M.mapWithKey focusLength $ foldl toBoxes mainmap inp
    print $ sum focusPowers
    where
        focusLength :: Int -> OM.OMap String Int -> Int
        focusLength key omap = (key+1) * foldl (\acc (i,len) -> acc + (i*len)) 0 (zip [1..] $ map snd $ OM.assocs omap)

        toBoxes :: M.Map Int (OM.OMap String Int) -> String -> M.Map Int (OM.OMap String Int)
        toBoxes acc str = do
            if '-' `elem` str then do
                let h = hash $ init str
                let newOmap = OM.alter (const Nothing) (init str) $ acc M.! h
                M.update (const $ Just newOmap) h acc
            else do
                let [label,n] = splitOn "=" str
                let h = hash label
                let newOmap = OM.alter (\_ -> Just (read n)) label $ acc M.! h
                M.update (const $ Just newOmap) h acc
