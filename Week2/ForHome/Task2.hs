main:: IO()
main = do 
    print $ sumDigitsIter 12345 == 15
    print $ sumDigitsIter 123 == 6

sumDigitsIter:: Int -> Int
sumDigitsIter num = sum num
    where
        sum:: Int -> Int 
        sum n 
            | n < 10 = n 
            | otherwise = mod n 10 + (sum (div n 10)) 