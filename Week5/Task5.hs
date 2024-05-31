import Data.Char
import Data.List
main::IO()
main = do
    print $ (applyN (\x -> 2 * x) 5) 2 == 64
    print $ (applyN (\x -> div x 10) 2) 100 == 1

applyN:: (Int -> Int) -> Int -> (Int -> Int)
applyN f n = (\ x -> helper x n)
    where
        helper::Int -> Int -> Int
        helper  num times
            | times < 1 = error "Invalid times"
            | times  == 1 = f num
            | otherwise =  helper (f num) (times - 1) 

-- Is there a way to be one liner task
-- How to write more complex lambda functions with guards

-- Description:

-- Define a function that takes a single argument function and returns it applied n times.

-- Acceptance criteria:

-- All tests pass.
-- Typeclasses are used.
-- Add one new test case. Place a comment after it with the words my test.