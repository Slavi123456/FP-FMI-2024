main::IO()
main = do
    print $ removeFirstOccurrence 16366 5 == 16366
    print $ removeFirstOccurrence 110 1 == 10
    print $ removeFirstOccurrence 15365 5 == 1536
    print $ removeFirstOccurrence 15360 0 == 1536
    print $ removeFirstOccurrence 15300 0 == 1530
    print $ removeFirstOccurrence 15365 1 == 5365
    print $ removeFirstOccurrence 35365 3 == 3565
    print $ removeFirstOccurrence 1212 1 == 122
    print $ removeFirstOccurrence 1212 2 == 121
    print $ removeFirstOccurrence (removeFirstOccurrence 1212 1) 1 == 22

removeFirstOccurrence:: Int -> Int -> Int
removeFirstOccurrence num x = reverse $ helper num 0
    where
        helper:: Int -> Int -> Int
        helper n res 
            | n < 10 = res * 10 + n
            | mod n 10 == x = res
            | otherwise = helper (mod n 10) (res * 10 + (mod n 10))