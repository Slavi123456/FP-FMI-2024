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

    print $ simplePaths [(1, [2, 3]), (2, [3]), (3, []), (4, [])] 2 5 -- == [[2,3]]
    -- print $ generatePath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 2
    -- print $ neighbors [(1, [2, 3]), (2, [3]), (3, []), (4, [])] 2
    -- print $ map (\(x,ys) -> [x] ++ ys)  [(1, [2, 3]), (2, [3]), (3, []), (4, [])]
    -- print $ nodes [(1, [2, 3]), (2, [3]), (3, []), (4, [])]
    -- print $ elem 5 $ nodes [(1, [2, 3]), (2, [3]), (3, []), (4, [])]

type Node = Int
type Path = [Node]
type Graph = [(Node, [Node])]

generatePath:: Graph -> Int -> Node  -> [Path]
generatePath gr k n = helper (fst $ head gr) k [] [[]]
    where 
        helper:: Node -> Int ->Path -> [Path] -> [Path]
        helper par max res final 
            | length res >= max = (final ++ [res])
            | null $ neighbors gr par = (final ++ [res ++ [par]])
            | otherwise = concat $ map (\x -> helper x max (res ++ [par]) final) (neighbors gr par)


nodes::Graph -> [Node]
nodes gr = nub $ concat $ map (\(x,ys) -> [x] ++ ys) gr

simplePaths::Graph -> Int -> Node -> [Path]
simplePaths gr k n 
    | elem n (nodes gr) == False = error "There is no such node" 
    | otherwise = nub $ filter (\ns -> length ns == k + 1)(generatePath gr (k+1) n)

neighbors:: Graph -> Node -> [Node]
neighbors gr n = head $ [childred |(parent, childred) <- gr, parent == n]


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

-- generatePath:: Graph -> Node -> Int -> [Path]
-- generatePath gr n k = helper (fst $ head gr) [] [[]]
--     where 
--         helper:: Node -> Path -> [Path] -> [Path]
--         helper par res final 
--             | null $ neighbors gr par = (final ++ [res ++ [par]])
--             | otherwise = concat $ map (\x -> helper x (res ++ [par]) final) (neighbors gr par)


-- Description:

-- Define a function that accepts a graph, a whole number k and a node n. Return all the paths starting 
-- from n that are k nodes long. If the node is not present, throw an error.

-- Use the following types: