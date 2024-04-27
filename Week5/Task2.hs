import Data.Char
import Data.List
main::IO()
main = do
    print $ getPalindromes 132465 == 8
    print $ getPalindromes 654546 == 8
    print $ getPalindromes 100001 == 100012
    print $ getPalindromes 21612 == 21614
    print $ getPalindromes 26362 == 26364
    --print $ sort $ [z | z <- [2 .. 132466], mod 132465 z == 0]     
    --print $ last $ sort $ [z | z <- [2 .. 132466], mod 132465 z == 0]

--copy paste
isPalindrome:: Int -> Bool
isPalindrome num = num == rev num 0
    where 
        rev::Int -> Int -> Int
        rev n res 
            | n < 10 = n + res * 10
            | otherwise = rev (div n 10) (res * 10 + mod n 10) 
            
getPalindromes::Int -> Int
getPalindromes n = (head $ helper n) + (last $ helper n)
    where 
        helper:: Int -> [Int]
        helper x = sort $ [z | z <- [2 .. (n + 1)], mod n z == 0, isPalindrome z == True]     


-- Description:

-- Define a function that returns the sum of the smallest and greatest palindrome divisors of a natural number greater than 1.

-- Acceptance criteria:

-- All tests pass.
-- Add one new test case. Place a comment after it with the words my test.