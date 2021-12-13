import Data.List

main :: IO ()
main = do
    input <- readFile "21dec.txt"
    let foods = [(map (takeWhile (/=',')) $ words $ takeWhile (/=')') $ dropWhile (/=' ') $ dropWhile (/='(') x, 
                  words $ takeWhile (/='(') x) | x <- lines input]
    let allergens      = nub [x | f <- foods, let (a,_) = f, x <- a]
    let allIngredients = [x | f <- foods, let (_,i) = f, x <- i]
    let lists = [(allergen, [i | i <- nub allIngredients, i `elemAll'` [ingredients | (a,ingredients) <- foods, allergen `elem'` a]]) | allergen <- allergens]

    let canContain = nub [i | (_,ingredients) <- lists, i <- ingredients]

    print $ length [i | i <- allIngredients, i `notElem` canContain]
    print $ show canContain
    writeFile "21dec-debug.txt" $ show lists


-- elemAll :: String -> [[String]] -> Bool
-- elemAll _     []     =  False
-- elemAll match [x]    =  match `elem` x
-- elemAll match (x:xs) = (match `elem` x) && elemAll match xs

elemAll' :: String -> [[String]] -> Bool
elemAll' _     [] = False
elemAll' match xs = foldr (\x ac -> match `elem'` x && ac) True xs

elem' :: Eq a => a -> [a] -> Bool
elem' x = foldr (\k ac -> k==x || ac) False