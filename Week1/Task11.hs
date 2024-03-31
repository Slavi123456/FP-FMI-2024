main::IO()
main = do
    print $ rev 1 == 1
    print $ rev 123 == 321
    print $ rev 987654321 == 123456789
    print $ rev 141561 == 165114
    --my test
    
rev:: Int -> Int
rev x
    | x < 0 = -1 --for invalid input
    | x < 10 = x
    |otherwise = revResult x 0 

revResult:: Int -> Int -> Int
revResult x y 
    | x < 10 = y * 10 + mod x 10
    | otherwise = revResult (div x 10) (y * 10 + mod x 10) 