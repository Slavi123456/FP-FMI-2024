main::IO()
main = do
    print $ sumDivisibleNumbers 50 10 5 == 290
    print $ sumDivisibleNumbers 0 10 5 == 5
    print $ sumDivisibleNumbers 0 100 5 == 990
    print $ sumDivisibleNumbers 100 0 5 == 990
    print $ sumDivisibleNumbers 3 3000 7

sumDivisibleNumbers::Int ->Int ->Int ->Int
sumDivisibleNumbers br1 br2 del = helper (min br1 br2) (max br1 br2) 0 
    where 
        helper::Int ->Int ->Int ->Int
        helper n border res 
            | n > border = res
            | mod (sumDigitsIter n) del == 0 = helper (n+1) border (res + n)
            | otherwise = helper (n+1) border res

sumDigitsIter:: Int -> Int
sumDigitsIter num = sum num
    where
        sum:: Int -> Int 
        sum n 
            | n < 10 = n 
            | otherwise = mod n 10 + (sum $ div n 10) 

-- Description:

-- Define a function sumDivisibleNumbers start finish k that returns the sum of all numbers
-- from the interval [start, finish] whose digits sum up to a number that is evenly divisible by k.