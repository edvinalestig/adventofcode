import Data.List.Split (splitOn)
import Data.List (nub)
import GHC.Unicode (isLower)

type Node = String
type Edge = [Node]

main :: IO ()
main = do
    input <- readFile "input.txt"
    let instructions = lines input

    print "Part 1"
    print $ part1 instructions
    print "Part 2"
    print $ part2 instructions


part1 :: [String] -> Int
part1 input = search "start" (map (splitOn "-") input) []


isSmall :: Node -> Bool
isSmall []    = error "Empty string"
isSmall (c:_) = isLower c

search :: Node -> [Edge] -> [Node] -> Int
search "end"       _     _                 = 1
search currentNode edges visitedSmallNodes = do
    let connected = getConnectedNodes currentNode edges
    let nextSteps = filter (`notElem` visitedSmallNodes) connected
    let visited = [currentNode | isSmall currentNode] ++ visitedSmallNodes
    sum $ map (\x -> search x edges visited) nextSteps


getConnectedNodes :: Node -> [Edge] -> [Node]
getConnectedNodes node edges = [n1 | [n1,n2] <- edges, n2 == node] ++ [n2 | [n1,n2] <- edges, n1 == node]

--------------------

part2 :: [String] -> Int
part2 input = do
    let nodes = filter (\x -> x /= "start" && x /= "end") $ nub $ concatMap (splitOn "-") input 
    let edges = map (splitOn "-") input

    length . nub $ concatMap (part2' edges) nodes

part2' :: [Edge] -> Node -> [[Node]]
part2' edges = search2 "start" edges ["start"] []

isNotVisited :: [Node] -> Node -> Node -> Bool
isNotVisited visited extraNode node
    | node == extraNode = length (filter (==node) visited) < 2
    | otherwise         = node `notElem` visited

search2 :: Node -> [Edge] -> [Node] -> [Node] -> Node -> [[Node]]
search2 "end"       _     _                 path _     = [path]
search2 currentNode edges visitedSmallNodes path node2 = do
    let connected = getConnectedNodes currentNode edges
    let nextSteps = filter (isNotVisited visitedSmallNodes node2) connected
    let visited = [currentNode | isSmall currentNode] ++ visitedSmallNodes
    concatMap (\x -> search2 x edges visited (currentNode:path) node2) nextSteps