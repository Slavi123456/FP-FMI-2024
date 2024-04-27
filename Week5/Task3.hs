import Data.Char
import Data.List
main::IO()
main = do
    print $ isArithmetic [3] == True
    print $ isArithmetic [3, 5] == True
    print $ isArithmetic [1, 2, 3, 4, 5] == True
    print $ isArithmetic [3, 5, 7, 9, 11] == True
    print $ isArithmetic [3, 5, 8, 9, 11] == False
    print $ isArithmetic [3, 5, 9, 9, 11] == False
    
    --print $ [3, 5 .. 11] == [3, 5, 7, 9, 11]
    --print $ [3, 5 .. 5]

isArithmetic:: [Int] -> Bool
isArithmetic [] = True
isArithmetic [_] = True
isArithmetic xs = xs == [(xs!!0), (xs!!1) .. (last xs)]

-- Description:

-- Define a predicate that checks whether a sequence of numbers forms an arithmetic progression.

-- Hint: You may find the operator !! useful 😇.

-- Acceptance criteria:

-- All tests pass.
-- Add one new test case. Place a comment after it with the words my test.
-- No if-else statements are present.
-- No guards that return True and/or False are present.