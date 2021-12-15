import Data.Heap ( insert, singleton, view, MinPrioHeap, partition, viewHead )
import Data.Char ( digitToInt )

data Node  = Node Int Int Int deriving (Show, Eq, Ord)
--             cost^  x^  y^

-- Simple binary search tree
data BST = Leaf | Branch BST Node BST deriving (Show, Eq)

insertBST :: Node -> BST -> BST
insertBST node Leaf = Branch Leaf node Leaf
insertBST node (Branch left n right)
    | node < n  = Branch (insertBST node left) n right
    | otherwise = Branch left n (insertBST node right)

elemBST :: Node -> BST -> Bool
_    `elemBST` Leaf = False
node `elemBST` (Branch left n right)
    | n == node = True
    | node < n  = elemBST node left
    | otherwise = elemBST node right

notElemBST :: Node -> BST -> Bool
node `notElemBST` bst = not $ node `elemBST` bst
---------


main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = map (map digitToInt) $ lines input


    putStrLn "-- Part 1 --"
    print $ run instructions 
    putStrLn "-- Part 2 --"
    print $ run $ expand instructions

-- We need the shortest path -> Dijkstra!


makeNode :: [[Int]] -> Int -> Int -> Node
makeNode matrix x y = Node ((matrix!!y)!!x) x y

makeNeighbours :: [[Int]] -> Int -> Int -> [Node]
makeNeighbours m x y
    | x == 0   && y == 0   = [makeNode m (x+1) y, makeNode m x (y+1)]
    | x == len && y == 0   = [makeNode m (x-1) y, makeNode m x (y+1)]
    | x == 0   && y == len = [makeNode m (x+1) y, makeNode m x (y-1)]
    | x == len && y == len = [makeNode m (x-1) y, makeNode m x (y-1)]
    | x == 0               = [makeNode m (x+1) y, makeNode m x (y-1), makeNode m x (y+1)]
    | x == len             = [makeNode m (x-1) y, makeNode m x (y-1), makeNode m x (y+1)]
    | y == 0               = [makeNode m (x-1) y, makeNode m (x+1) y, makeNode m x (y+1)]
    | y == len             = [makeNode m (x-1) y, makeNode m (x+1) y, makeNode m x (y-1)]
    | otherwise            = [makeNode m (x-1) y, makeNode m (x+1) y, makeNode m x (y-1), makeNode m x (y+1)]
    where len = length m - 1

--------------------------

dijkstra :: Node -> Node -> [[Int]] -> Int
dijkstra start = dijkstraStep Leaf (singleton (0,start))

dijkstraStep :: BST -> MinPrioHeap Int Node -> Node -> [[Int]] -> Int
dijkstraStep visited heap targetNode graph 
    | Node w x y == targetNode     = cost
    | Node w x y `elemBST` visited = dijkstraStep visited heapTail targetNode graph
    | otherwise                    = do
        let newHeap = addToHeap heapTail cost $ filter (`notElemBST` visited) $ makeNeighbours graph x y
        dijkstraStep (insertBST (Node w x y) visited) newHeap targetNode graph
    where 
        Just ((cost,Node w x y), heapTail) = view heap

addToHeap :: MinPrioHeap Int Node -> Int -> [Node] -> MinPrioHeap Int Node
addToHeap heap _    []                       = heap
addToHeap heap cost ((Node weight x y):rest) = addToHeap (insertAndFilter (cost+weight,Node weight x y) heap) cost rest

-- I don't know if this makes it quicker or slower... I don't know how well the heap handles a lot of data
insertAndFilter :: (Int,Node) -> MinPrioHeap Int Node -> MinPrioHeap Int Node
insertAndFilter (cost,node) heap = do
    let (current,restOfHeap) = partition ((==node) . snd) heap
    if null current then
        insert (cost,node) restOfHeap
    else do
        let Just (cost2,_) = viewHead current
        insert (min cost cost2, node) restOfHeap 



----------------

run :: [[Int]] -> Int
run matrix = dijkstra (makeNode matrix 0 0) (makeNode matrix len len) matrix
    where len = length matrix - 1

expand :: [[Int]] -> [[Int]]
expand m = expandDown $ map expandRight m

expandRight :: [Int] -> [Int]
expandRight m = m ++ mp 1 m ++ mp 2 m ++ mp 3 m ++ mp 4 m

expandDown :: [[Int]] -> [[Int]]
expandDown m = m ++ map (mp 1) m ++ map (mp 2) m ++ map (mp 3) m ++ map (mp 4) m

mp :: Int -> ([Int] -> [Int])
mp n = map ((+1) . (`mod` 9) . (+(n-1)))