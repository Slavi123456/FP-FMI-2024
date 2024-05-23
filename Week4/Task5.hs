import Data.Char
import Data.List
main::IO()
main = do
    print $ isAscending 0 == True
    print $ isAscending 10 == False
    print $ isAscending 123 == True
    print $ isAscending 1233 == True
    print $ isAscending 12332 == False 
    --my test
    print $ isAscending 904217 == False       
    --print $ group $ show 10
    --print $ sort $ group $ show 10
    --print $ (minimum $ group $ show 51) == (head $ group $ show 15)
    
isAscending:: Int -> Bool
isAscending x = func x == (sort $ func x)

func:: Int -> [String]
func x = group $ show x
-- Description:

-- Define a function that checks whether the digits of a non-negative number are ordered in non-decreasing order.

-- Acceptance criteria:

-- All tests pass.
-- The solution does not contain if-then-else statements.
-- Add one new test case. Place a comment after it with the words my test.
-- The implementation should be on one line.
-- The implementation does not include div and/or mod.