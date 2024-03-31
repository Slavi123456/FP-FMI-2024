main:: IO()
main = do
    print $ removeD 1 656 == 656
    print $ removeD 5 656 == 66
    print $ removeD 6 656 == 5
    print $ removeD 0 606 == 66
    print $ removeD 0 600 == 6
    print $ removeD 6 600 == 0
    print $ removeD 2 1234 == 134 

removeD:: Int -> Int -> Int 
removeD n num = rev 0 (help num 0) 
    where
        help:: Int -> Int -> Int 
        help copy res
            | copy <= 0 = res
            | mod copy 10 /= n = help (div copy 10) (10 * res + mod copy 10)
            | otherwise = help (div copy 10) res

rev::Int -> Int -> Int
rev res n 
    | n < 10 = n + res * 10
    | otherwise = rev (res * 10 + mod n 10) (div n 10)