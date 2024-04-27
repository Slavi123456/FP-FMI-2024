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

-- Как се прави задачата в един ред? (Да създадеш списък от функциите и да итерираш до празен списък)
-- Как се пишат guards в lambda functions

-- Description:

-- Define a function that takes a single argument function and returns it applied n times.

-- Acceptance criteria:

-- All tests pass.
-- Typeclasses are used.
-- Add one new test case. Place a comment after it with the words my test.