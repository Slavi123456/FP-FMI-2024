main::IO()
main = do
    -- print $ warmerAfter [20,21,20,19,18,20,25,24,23,20,26] == [1,5,4,2,1,1,4,3,2,1,0]
    -- print $ warmerAfter [0,10,20,30] == [1,1,1,0]
    -- print $ warmerAfter [21,22,23] ==  [1,1,0]
    -- print $ warmerAfter [23,24,25,21,19,23,26,23] == [1,1,4,2,1,1,0,0]

warmerAfter::[Double] -> [Int]
warmerAfter [] = []
warmerAfter xs = daysNum xs 
    where
        daysNum:: [Double] ->[Int]
        daysNum [] = []
        daysNum [_] = [0] 
        daysNum (x:xs) = helper xs 0 : daysNum xs
            where 
                helper:: [Double] -> Int -> Int
                helper ts res
                    | null ts = 0 
                    | head ts > x = res + 1
                    | res > length ts = 0
                    | otherwise = helper (tail ts) (res + 1)