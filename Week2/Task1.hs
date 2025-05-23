main:: IO()
main = do
    print $ isPalindrome 1 == True
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False
    print $ isPalindrome 121 == True
    print $ isPalindrome 12 == False
    print $ isPalindrome 120 == False
    print $ isPalindrome 12321 == True
    print $ isPalindrome 1221 == True

isPalindrome:: Int -> Bool
isPalindrome num = num == rev num 0
    where 
        rev::Int -> Int -> Int
        rev n res 
            | n < 10 = n + res * 10
            | otherwise = rev (div n 10) (res * 10 + mod n 10) 
            
            