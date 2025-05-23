import Data.Char
import Data.List
main::IO()
main = do
    -- you may get slightly different results eg. [3, 4, 5] on test 1 <- not a problem
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] -- == [4, 3, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]
    -- print $ nodes [(1, 2, 3), (3, 4, 5), (5, 6, 9)]
    -- print $ parents [(1, 2, 3), (3, 4, 5), (5, 6, 9)]

type Node = Int
type BTNode =(Node, Node, Node)
type BinaryTree = [BTNode]

listLeaves:: BinaryTree -> [Node]
listLeaves bt = [x | x <- nodes bt, elem x (parents bt) == False]

parents::BinaryTree -> [Node]
parents bt = [parent | (parent, leftChild, rightChild) <- bt] 

nodes:: BinaryTree -> [Node]
nodes bt = nub $ concat $ map (\(x,y,z) -> [x,y,z]) bt 