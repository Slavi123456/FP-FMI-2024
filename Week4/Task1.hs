import Data.Char
import Data.List
main::IO()
main = do
    print $ mySumRecNonPM [] == 0
    print $ mySumRecNonPM [1, 2, 3] == 6

    print $ mySumRecPM [] == 0
    print $ mySumRecPM [1, 2, 3] == 6

    print $ mySumFunc [] == 0
    print $ mySumFunc [1, 2, 3] == 6

mySumRecNonPM:: (Num a, Fractional a) => [a] -> a
mySumRecNonPM xs
    | null xs = 0
    | otherwise = (+ head xs) $ mySumRecNonPM $ tail xs

mySumRecPM:: (Num a, Fractional a) => [a] -> a
mySumRecPM [] = 0
mySumRecPM (x:xs) = x + mySumRecPM xs

mySumFunc:: (Num a, Fractional a) => [a] -> a
mySumFunc = sum
--Description:

--Define a function that finds the sum of the elements in a list.

--All tests pass.
--The solution does not contain if-then-else statements.
--Add one new test case. Place a comment after it with the words my test.
--mySumRecNonPM is defined using a linearly recursive process without pattern matching.
--mySumRecPM is defined using a linearly recursive process with pattern matching.
--mySumFunc is defined using functions that work with lists.
