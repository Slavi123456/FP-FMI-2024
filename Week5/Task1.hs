import Data.Char
import Data.List
main::IO()
main = do 
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]
    
    print $ elem '7' (show 175435423777)
isPrime:: Int -> Bool
isPrime num = num > 1 && null [x |x <- [2 .. num - 1], mod num x == 0]

getPrimesLC:: Int -> Int -> [Int]
getPrimesLC x y = [z | z <- [min x y.. max x y ], isPrime z, doesContain z 7]

getPrimesHOF:: Int -> Int -> [Int]
getPrimesHOF x y = filter (\z -> isPrime z && doesContain z 7) [min x y.. max x y ]

doesContain::Int -> Int -> Bool
doesContain num n = elem (intToDigit n) (show num)
-- Description:

-- Define a function that returns the prime numbers in the range (x, y) that contain the digit 7.

-- Acceptance criteria:

-- All tests pass.
-- Add one new test case. Place a comment after it with the words my test.
-- getPrimesLC is defined using list comprehension.
-- getPrimesLC is defined on a single line of code.
-- getPrimesHOF is defined using higher order functions.
-- getPrimesHOF is defined on a single line of code.
    