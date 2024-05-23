import Data.Char
import Data.List
main::IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392 -- n = 5, d = 2
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462
    print $ sumSpecialPrimes 10 3 == 462
    --my test
    print $ sumSpecialPrimes 3 4 == 131

isPrime:: Int -> Bool
isPrime num = num > 1 && null [x |x <- [2 .. num - 1], mod num x == 0]

sumSpecialPrimes:: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n [x | x <- [2 .. ], isPrime x, elem (intToDigit d) (show x)]

-- sumSpecialPrimes:: Int -> Int -> Int
-- sumSpecialPrimes n d = let t = show d in sum $ take n [x | x <- [2 .. ], isPrime x, elem t (group $ nub $ show x)]

-- Description:

-- Define a function that returns the sum of the first n prime numbers that contain a digit d.

-- Hint: You can define an infinite list by using this construction: [1 .. ]. This defines a list with no upper bound.
-- Now, use functions such as 'take' and 'drop' (refer to them in 'notes.txt') to get only the first 'n' numbers that 
-- satisfy the condition. For example: take ??? [1 .. ].

-- Acceptance criteria:

-- All tests pass.
-- The solution does not contain if-then-else statements.
-- Add one new test case. Place a comment after it with the words my test.
-- The solition uses higher order functions.
-- The solition is defined on one line of code.