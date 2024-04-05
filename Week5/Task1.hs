import Data.Char
import Data.List
main::IO()
main = do 
    print $ getPrimesLC 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesLC 100 1 == [7,17,37,47,67,71,73,79,97]

    print $ getPrimesHOF 1 100 == [7,17,37,47,67,71,73,79,97]
    print $ getPrimesHOF 100 1 == [7,17,37,47,67,71,73,79,97]
    
    --print $ elem "7" (group $ nub $ show 17)
--copy paste
isPrime:: Int -> Bool
isPrime 2 = True
isPrime num 
    | num <= 1 = False
    | otherwise = help (num - 1)
    where 
        help:: Int -> Bool
        help x 
            | x == 2 = mod num x /= 0 
            | otherwise = mod num x /= 0 && help (x - 1)

getPrimesLC:: Int -> Int -> [Int]
getPrimesLC x y = [z | z <- [(min x y).. (max x y )], isPrime z == True, elem "7" (group $ nub $ show z) == True]

getPrimesHOF:: Int -> Int -> [Int]
getPrimesHOF x y = filter (\z -> isPrime z == True && elem "7" (group $ nub $ show z) == True) [(min x y).. (max x y )]
-- Description:

-- Define a function that returns the prime numbers in the range (x, y) that contain the digit 7.

-- Acceptance criteria:

-- All tests pass.
-- Add one new test case. Place a comment after it with the words my test.
-- getPrimesLC is defined using list comprehension.
-- getPrimesLC is defined on a single line of code.
-- getPrimesHOF is defined using higher order functions.
-- getPrimesHOF is defined on a single line of code.
    