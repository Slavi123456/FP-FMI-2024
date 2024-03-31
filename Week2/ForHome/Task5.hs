main:: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True

areAmicable:: Int -> Int -> Bool 
areAmicable num sum = helper (num - 1) == sum
    where
        helper:: Int -> Int 
        helper n 
            | n == 1 = 1
            | mod num n == 0 = n + helper (n - 1)
            | otherwise = helper (n - 1)