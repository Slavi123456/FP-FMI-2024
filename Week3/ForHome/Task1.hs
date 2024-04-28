main::IO()
main = do
    print $ removeFirstOccurrence 16366 5 == 16366
    print $ removeFirstOccurrence 110 1  == 10
    print $ removeFirstOccurrence 15365 5 == 1536
    print $ removeFirstOccurrence 15360 0 == 1536
    print $ removeFirstOccurrence 15300 0 == 1530
    print $ removeFirstOccurrence 15365 1 == 5365
    print $ removeFirstOccurrence 35365 3 == 3565
    print $ removeFirstOccurrence 1212 1 == 122
    print $ removeFirstOccurrence 1212 2 == 121
    print $ removeFirstOccurrence (removeFirstOccurrence 1212 1) 1 == 22

removeFirstOccurrence:: Int -> Int -> Int
removeFirstOccurrence num x
    | helper num > len num = num
    | otherwise = mod num (10 ^ (helper num - 1)) + (remDigit num $ helper num) 
    where
        helper:: Int -> Int
        helper 0 = 1
        helper n
            | mod n 10 == x = 1
            | otherwise = (+1) $ helper $ div n 10

remDigit::Int -> Int -> Int
remDigit num idx =  (* (10 ^ (idx - 1))) $ div num $ (^) 10 idx

len::Int -> Int 
len x 
    | x < 10 = 1
    | otherwise = (+1) $ len $ div x 10

-- Description:

-- Define a function that removes the first occurrence of a digit in a number by going from right to left.