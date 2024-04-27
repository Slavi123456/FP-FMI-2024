import Data.Char
import Data.List
main::IO()
main = do
    print $ specialSum 1 100 == 195 -- 61, 65, 69
    print $ (round $ fromIntegral $ div (61 - 1) 4 ) == 15
    --print $ mod 95 4
    --print [z | z <- [1.. 100], elem "6" (group $ nub $ show z) == True, mod (z - 1) 4 == 0]
specialSum:: Int -> Int -> Int
specialSum x y = sum $ [z | z <- [x .. y], elem "6" (group $ nub $ show z) == True, mod (z - 1) 4 == 0]

-- Description:

-- Define a function that returns the sum of the special numbers in the interval [x, y] (x <= y). A number is special if it contains 6 and can be expressed as 4k + 1, where k is a whole number.

-- Acceptance criteria:

-- All tests pass.
-- Add one new test case. Place a comment after it with the words my test.
-- The task is solved on a single line of code.