import Data.Char
import Data.List
main::IO()
main = do
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 2, 4] == True
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 3, 4] == False
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [2, 3] == True
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [3, 1] == False
    print $ isChild 3 [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]

type Node = Int
type Children = [Node]
type Graph = [(Node, Children)]
type Path = [Node]

isPath:: Graph -> Path -> Bool
isPath gr p = False

isChild:: Node-> Graph -> Bool
isChild n gr = elem n $ nub $concat $ [children | (parent, children) <- gr ]
-- Description:

-- Define a function that checks whether a given path is valid in a graph g = (1, [2, 3]), (2, [3, 4]), (3, []), (4, []).

-- Acceptance criteria:

-- All tests pass.