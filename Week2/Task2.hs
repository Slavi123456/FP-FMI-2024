main:: IO()
main = do
    print $ sumDigitsRec 12345 == 15
    print $ sumDigitsRec 123 == 6

sumDigitsRec:: Int -> Int
sumDigitsRec n 
    | n < 10 = n
    | otherwise = mod n 10 + (sumDigitsRec $ div n 10)
    --otherwise = (+ mod n 10) $ sumDigitsRec $ div n 10
