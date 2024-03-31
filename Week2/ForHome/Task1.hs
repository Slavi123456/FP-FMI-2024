main:: IO()
main = do
    --print $ countDigitsIter (-13) -- error "n was negative"
    print $ countDigitsIter 12345 == 5
    print $ countDigitsIter 123 == 3

    --print $ countDigitsRec (-13) -- error "n was negative"
    print $ countDigitsRec 12345 == 5
    print $ countDigitsRec 123 == 3

countDigitsIter:: Int -> Int 
countDigitsIter num 
    | num < 0 = error "n was negative"
    |otherwise = digitCount num 
    where 
        digitCount:: Int -> Int
        digitCount n
            | n < 10 = 1
            | otherwise = 1 + digitCount (div n 10)        

countDigitsRec:: Int -> Int 
countDigitsRec n 
    | n < 10 = 1
    |otherwise = 1 + countDigitsRec (div n 10)