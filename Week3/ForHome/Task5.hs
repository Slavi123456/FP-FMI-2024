main::IO()
main = do
    print $ p 1 == 1
    print $ p 2 == 5
    print $ p 3 == 12
    print $ p 4 == 22
    print $ p 5 == 35
    print $ p 6 == 51
    --print $ cpyPoint 3 3 4 0

p::Int -> Int
p 2 = 5
p 1 = 1
p n = helper n 0 - cpyPoint 3 3 n 0
    where
        helper::Int -> Int -> Int 
        helper num res
            | num <= 2 = res +  5 
            |otherwise = helper (num - 1) (res + 5 * (num - 1))

cpyPoint::Int -> Int -> Int -> Int  -> Int
cpyPoint x idx n res
    | idx >= n = res + x
    | otherwise = cpyPoint (x + 2) (idx + 1) n (res + x)