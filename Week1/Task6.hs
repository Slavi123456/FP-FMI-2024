main::IO()
main = do
    print $ myGcdG 5 13 == 1
    print $ myGcdG 13 1235 == 13
    print $ myGcdG 425 75 == 25
    --my test
    print $ myGcdPM 5 13 == 1
    print $ myGcdPM 13 1235 == 13
    print $ myGcdPM 143 11 == 31
    --my test
    
myGcdG:: Int -> Int -> Int 
myGcdG x y 
    | x == 0 = y
    | y == 0 = x
    | otherwise = myGcdG y (x - div x y*y)

myGcdPM:: Int -> Int -> Int
myGcdPM 0 y = y
myGcdPM x 0 = x
myGcdPM x y = myGcdPM y (x - y * div x y)