import Data.Char
import Data.List

main::IO()
main = do
    print $ nodes [(1, 2), (1, 3), (2, 3), (2, 4)] -- == [1, 2, 3, 4]

    print $ neighbors 2 [(1, 2), (1, 3), (2, 3), (2, 4)] == [3, 4]
    print $ neighbors 4 [(1, 2), (1, 3), (2, 3), (2, 4)] == []

    print $ adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)] -- == [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]

nodes:: Graph -> [Node]
nodes gr = nub $ concat $ map (\(x,y) -> [x,y]) gr

neighbors::Node-> Graph -> Nodes
neighbors n gr = [child | (parent, child) <- gr, parent == n]

adjacencyList::Graph -> [(Node, Nodes)]
adjacencyList gr = [(parent, neighbors parent gr) | parent <- (nodes gr)] 

type Node = Int
type Nodes = [Node]
type Edge = (Node,Node)
type Graph = [Edge]

-- Given a directed graph g with edges [(1, 2), (1, 3), (2, 3), (2, 4)] define the following functions:

-- nodes - returns all the nodes of "g";
-- neighbors - accepts a node and returns its neighbors;
-- adjacencyList - returns the children of every parent.
-- Acceptance criteria:

-- All tests pass.
-- Appropriate types are created for the graph structure.