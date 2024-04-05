import Data.Char
import Data.List
main::IO()
main = do
    print $ primesInRangeLC 1 100 == []
    print $ primesInRangeLC 998 1042 == [1009,1013,1019,1021,1031,1033,1039]
    print $ primesInRangeLC 120 666 == [127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661]
    print $ primesInRangeLC 420 240 == [241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419]
    print $ primesInRangeHOF 1 100 == []
    print $ primesInRangeHOF 998 1042 == [1009,1013,1019,1021,1031,1033,1039]
    print $ primesInRangeHOF 120 666 == [127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661]
    print $ primesInRangeHOF 420 240 == [241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419] 

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

primesInRangeLC:: Int -> Int -> [Int]
primesInRangeLC x y = [ z | z <- [(min x y).. (max x y)], z > 100, isPrime z == True ]

primesInRangeHOF:: Int -> Int -> [Int]
primesInRangeHOF x y = filter (\ z -> isPrime z == True) $ filter (\ n -> n > 100) [(min x y).. (max x y)]
-- Description:

-- Define a function that returns a list of the prime numbers with at least three digits in a given interval.

-- Acceptance criteria:

-- All tests pass.
-- The solution does not contain if-then-else statements.
-- Add one new test case. Place a comment after it with the words my test.
-- primesInRangeLC uses list comprehension in one line of code.
-- primesInRangeHOF uses higher order functions in one line of code.