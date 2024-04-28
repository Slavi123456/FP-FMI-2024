main::IO()
main = do
        print $ sortN 1714 == 7411
        print $ sortN 123450 == 543210
        print $ sortN 123405 == 543210
        print $ sortN 123045 == 543210
        print $ sortN 120345 == 543210
        print $ sortN 102345 == 543210
        print $ sortN 8910 == 9810
        print $ sortN 321 == 321
        print $ sortN 29210 == 92210
        print $ sortN 1230 == 3210
        print $ sortN 55345 == 55543
        print $ sortN 14752 == 75421
        print $ sortN 329450 == 954320
        print $ sortN 9125 == 9521


sortN::Int -> Int
sortN num = helper num 0
    where
        helper::Int -> Int -> Int 
        helper n res
            | n < 10 = res * 10 + n
            |otherwise = helper (removeFirstOccurrence n (maxDig n)) (res * 10 + maxDig n)

maxDig::Int -> Int
maxDig num = helper num (mod num 10)
    where
        helper::Int ->Int -> Int
        helper n max
            | n <= 0 = max 
            | mod n 10 > max = helper (div n 10) (mod n 10)
            | otherwise = helper (div n 10) max

removeFirstOccurrence:: Int -> Int -> Int
removeFirstOccurrence num x
    | helper num > len num = num
    | otherwise = mod num (10 ^ (helper num - 1)) + remDigit num (helper num) 
    where
        helper:: Int -> Int
        helper n
            | n <= 0 = 1
            | mod n 10 == x = 1
            | otherwise = 1 + helper (div n 10)

remDigit::Int -> Int -> Int
remDigit num idx =  (* (10 ^ (idx - 1))) $ div num (10 ^ idx)

len::Int -> Int 
len x 
    | x < 10 = 1
    | otherwise = 1 + len (div x 10)

-- Description:

-- Define a function that sorts a number in descending order.