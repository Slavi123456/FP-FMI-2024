import Data.Char
import Data.List
main::IO()
main = do
    print $ sumUnevenHOF 5 50 == 621
    print $ sumUnevenHOF 50 1 == 625
    print $ sumUnevenHOF 564 565 == 565
    --my test 
    print $ sumUnevenLC 5 10 == 21

    print $ sumUnevenLC 5 50 == 621
    print $ sumUnevenLC 50 1 == 625
    print $ sumUnevenLC 564 565 == 565
    
    --my test 
    print $ sumUnevenLC 5 10 == 21

sumUnevenLC:: Int -> Int -> Int
sumUnevenLC x y = sum [z | z <- [min x y..max x y], odd z ]

sumUnevenHOF:: Int -> Int -> Int
sumUnevenHOF x y = sum $ filter (\ z -> odd z) [min x y..max x y]
-- Description:

-- Define a function that returns the sum of the uneven numbers in a range.

-- Acceptance criteria:

-- All tests pass.
-- The solution does not contain if-then-else statements.
-- Add one new test case. Place a comment after it with the words my test.
-- sumUnevenLC uses list comprehension in ONE line of code.
-- sumUnevenHOF uses higher order functions in ONE line of code.