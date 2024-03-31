main:: IO()
main = do
    print $ truncatablePrime 3797 == True -- 3797, 379, 37 and 3 are all prime
    print $ truncatablePrime 47 == False -- 47 is prime, but 4 is not
    print $ truncatablePrime 0 == False
    print $ truncatablePrime 1 == False
    print $ truncatablePrime 2 == True
    print $ truncatablePrime 37397 == True
    print $ truncatablePrime 1399 == False -- 1 is not prime
    print $ truncatablePrime 1733 == False -- 1 is not prime
    print $ truncatablePrime 1913 == False -- 1 is not prime
    print $ truncatablePrime 1931 == False -- 1 is not prime
    print $ truncatablePrime 1933 == False -- 1 is not prime
    print $ truncatablePrime 1973 == False -- 1 is not prime
    print $ truncatablePrime 19333 == False -- 1 is not prime
    print $ truncatablePrime 19739 == False -- 1 is not prime

truncatablePrime:: Int -> Bool
truncatablePrime 0 = False
truncatablePrime 1 = False
truncatablePrime num
    -- num <= 0 = True защо не работи
    | num < 10 = isPrime num
    | otherwise = isPrime num && truncatablePrime (div num 10)


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