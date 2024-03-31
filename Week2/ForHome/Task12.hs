main:: IO()
main = do
    print $ findSum 0 2 10 == 3578 -- 510 + 1022 + 2046
    print $ findSum 5 3 5 == 174 -- 26 + 50 + 98

findSum:: Int -> Int -> Int -> Int
findSum a b n = 3*a + b*(2^n + 3*help (n-3))
    where
        help:: Int -> Int
        help power
            | power == 0 = 1
            | otherwise = 2^power + help (power - 1)