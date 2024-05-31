-- Description:

-- Define a predicate that checks whether an element is present in a list.

-- Acceptance criteria:

-- All tests pass.
-- The solution does not contain if-then-else statements.
-- Add one new test case. Place a comment after it with the words my test.
-- isPresentRecNonPM is defined using a linearly recursive process without pattern matching.
-- isPresentRecPM is defined using a linearly recursive process with pattern matching.
-- isPresentFunc is defined using functions that work with lists.
import Data.List
import Data.Char
main:: IO()
main = do
    print $ isPresentRecNonPM 0 [] == False
    print $ isPresentRecNonPM 0 [1, 2, 3] == False
    print $ isPresentRecNonPM 0 [0, -1, 2] == True
    --my test 
    print $ isPresentRecNonPM 13 [0, -1, 2, 6 ,43, 432, 13] == True

    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True
    --my test 
    print $ isPresentRecPM 13 [0, -1, 2, 6 ,43, 432, 13] == True

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True
    --my test 
    print $ isPresentFunc 13 [0, -1, 2, 6 ,43, 432, 13] == True

--There is a problem when calling it lacks an accompanying binding
-- isPresentRecNonPM:: (Eq a) => a -> [a] -> Bool
-- isPresentRecNonPM _ [] = False
-- isPresentRecNonPM num (x:xs) = num == x || isPresentRecNonPM num xs

isPresentRecNonPM::  Int -> [Int] -> Bool
isPresentRecNonPM num xs = null [x | x <- xs, x == num] == False


--isPresentRecNonPM::  Int -> [Int] -> Bool
--isPresentRecNonPM num xs = if (null xs == False) then (if (num == head xs) then True else False || isPresentRecNonPM num (tail xs)) else False

isPresentRecPM:: Int -> [Int] -> Bool
isPresentRecPM _ [] = False
isPresentRecPM num (x:xs) = num == x || isPresentRecPM num xs

isPresentFunc:: Int -> [Int] -> Bool
isPresentFunc = elem