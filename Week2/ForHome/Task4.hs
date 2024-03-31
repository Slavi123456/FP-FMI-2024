main:: IO()
main = do
    print $ countOccurrences 121 1 == 2
    print $ countOccurrences 222 1 == 0
    print $ countOccurrences 100 0 == 2
    print $ countOccurrences 0 0 == 1

countOccurrences:: Int -> Int -> Int
countOccurrences num n
    | num == n = 1
    | num < 10 = 0
    | mod num 10 == n = 1 + countOccurrences (div num 10) n
    | otherwise =  countOccurrences (div num 10) n