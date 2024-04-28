main::IO()
main = do
    print $ p 1 == 1
    print $ p 2 == 5
    print $ p 3 == 12
    print $ p 4 == 22
    print $ p 5 == 35
    print $ p 6 == 51

p::Int -> Int
p 2 = 5
p 1 = 1
p n = helper n - cpyPoint 3 3 n
    where
        helper::Int -> Int 
        helper num 
            | num <= 2 = 5 
            |otherwise = 5 * (num - 1)  + helper (num - 1)

cpyPoint::Int -> Int -> Int -> Int
cpyPoint x idx n
    | idx >= n = x
    | otherwise = x + cpyPoint (x + 2) (idx + 1) n 