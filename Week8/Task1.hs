import Data.Char
import Data.List
main:: IO()
main = do
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 -- == [[1]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 -- == [[1, 2], [1, 3]]
    print $ simplePaths [(1, [2, 3, 4]), (2, [3, 4]), (3, []), (4, [])] 1 1 -- == [[1,2],[1,3],[1,4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 -- == [[1, 2, 3], [1, 2, 4]]
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2 -- == [[2,3],[2,4]]
    print $ simplePaths [(1, [2, 3]), (2, [3]), (3, []), (4, [])] 1 2 -- == [[2,3]]

    -- print $ generatePath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2
    -- print $ neighbors [(1, [2, 3]), (2, [3]), (3, []), (4, [])] 2
type Node = Int
type Path = [Node]
type Graph = [(Node, [Node])]

-- simplePaths:: Graph -> Int -> Node -> Path
-- simplePaths gr k n = helper 0 [(fst $ head gr)] []
--     where 
--         helper:: Int -> [Node] ->Path -> Path 
--         helper:: _ [] _ = []
--         helper num node res 
--             | num == k = res
--             |otherwise = helper (num+1) [(head $ neighbors gr node)]  (res ++ [node]) --filter (\(parent, childred) -> parent == node) gr 

-- generatePath:: Graph -> Node -> Int -> Path
-- generatePath gr n k = helper (fst $ head gr) 0 []
--     where 
--         helper:: Node -> Int -> Path -> Path
--         helper par num res 
--             | num == k = res 
--             | null $ neighbors gr par = res ++ [par]
--             | otherwise = helper (head $ neighbors gr par) (num + 1) (res ++ [par])

-- generatePath:: Graph -> Node -> [Path]
-- generatePath gr n = map (\x -> helper x [n]) (neighbors gr n)
--     where 
--         helper:: Node -> Path -> Path
--         helper par res  
--             | null $ neighbors gr par = res ++ [par]
--             | otherwise = helper (head $ neighbors gr par) (res ++ [par])

generatePath:: Graph -> Node -> Int -> [Path]
generatePath gr n k = helper (fst $ head gr) 1 [n]--map (\x -> helper x 1 [n]) (neighbors gr n)
    where 
        helper:: Node -> Int -> [Path] -> [Path]
        helper par num res 
            | num >= k = res 
            | null $ neighbors gr par = res ++ [par]
            | otherwise = map (\x -> helper x (num + 1) (res ++ [par])) (neighbors gr par)

simplePaths::Graph -> Int -> Node -> [Path]
simplePaths gr 0 n = [[n]]
simplePaths gr k n = generatePath gr n (k + 1) -- filter (\ns -> length ns == k + 1)(generatePath gr n)


neighbors:: Graph -> Node -> [Node]
neighbors gr n = head $ [childred |(parent, childred) <- gr, parent == n]

-- pathGenerator:: Graph -> [Path]
-- pathGenerator (g:gs) = [fst g] ++ helper 
--     where 
--         helper:: Node -> (Node, [Node])
-- Description:

-- Define a function that accepts a graph, a whole number k and a node n. Return all the paths starting 
-- from n that are k nodes long. If the node is not present, throw an error.

-- Use the following types: