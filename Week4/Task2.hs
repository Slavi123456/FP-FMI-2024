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

    print $ isPresentRecPM 0 [] == False
    print $ isPresentRecPM 0 [1, 2, 3] == False
    print $ isPresentRecPM 0 [0, -1, 2] == True

    print $ isPresentFunc 0 [] == False
    print $ isPresentFunc 0 [1, 2, 3] == False
    print $ isPresentFunc 0 [0, -1, 2] == True

--Dava problemi //0-та като какъв тип я възприема?
-- isPresentRecNonPM:: (Num a, Fractional a ) => a -> [a] -> Bool
-- isPresentRecNonPM num (x:xs)
--     | num == x = True
--     | otherwise = False || isPresentRecNonPM num xs

isPresentRecNonPM::  Int -> [Int] -> Bool
isPresentRecNonPM num xs
    | null xs = False
    | num == head xs = True
    | otherwise = False || isPresentRecNonPM num (tail xs)

isPresentRecPM:: Int -> [Int] -> Bool
isPresentRecPM _ [] = False
isPresentRecPM num (x:xs)
    | num == x = True
    | otherwise = False || isPresentRecPM num xs

isPresentFunc:: Int -> [Int] -> Bool
isPresentFunc _ [] = False
isPresentFunc x xs = elem x xs