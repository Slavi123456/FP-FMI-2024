main:: IO()
main = do
    print $ isInteresting 410 == True
    print $ isInteresting 212 == False
    print $ isInteresting 567 == False
    print $ isInteresting 70 == True 
    print $ isInteresting 5 == True 
    print $ isInteresting 4 == True 

isInteresting:: Int -> Bool
isInteresting x = (mod x (sumDigitsIter x)) == 0
    where
        sumDigitsIter:: Int -> Int
        sumDigitsIter num = sum num
            where
                sum:: Int -> Int 
                sum n 
                    | n < 10 = n 
                    | otherwise = mod n 10 + (sum (div n 10))