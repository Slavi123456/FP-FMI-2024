main:: IO()
main = do
    print $ sumPrimeDivs 0 == 0
    print $ sumPrimeDivs 6 == 5 -- 2 + 3
    print $ sumPrimeDivs 18 == 5 -- 2 + 3
    print $ sumPrimeDivs 19 == 19
    print $ sumPrimeDivs 45136 == 53

sumPrimeDivs:: Int -> Int 
sumPrimeDivs num = helper num
    where 
        helper:: Int -> Int
        --helper 2 = 2 
        helper dev 
            | dev <= 1 = 0
            | mod num dev == 0 && isPrime dev = dev + helper (dev - 1)
            | otherwise = helper (dev - 1)    


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