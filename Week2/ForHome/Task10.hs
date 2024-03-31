main:: IO()
main = do
    print $ countPalindromes 5 13 == 5 -- 6 7 8 9 11
    print $ countPalindromes 13 5 == 5 -- 6 7 8 9 11

countPalindromes:: Int -> Int -> Int
countPalindromes bd1 bd2 = help ((min bd1 bd2) + 1) 
    where    
        help:: Int -> Int 
        help num1 
            |num1 == max bd1 bd2 = 0
            |isPalindrome num1 = 1 + help (num1 + 1)
            |otherwise = help (num1 + 1) 


isPalindrome:: Int -> Bool
isPalindrome num = num == rev num 0
    where 
        rev::Int -> Int -> Int
        rev n res 
            | n < 10 = n + res * 10
            | otherwise = rev (div n 10) (res * 10 + mod n 10) 