main:: IO()
main = do
    print $ powRec 2 5 == 32
    print $ powRec 15 3 == 3375

    print $ powIter 2 5 == 32
    print $ powIter 15 3 == 3375

    --print $ powRec 2 0 == 1 -- should return an error (according to the task description)

powRec:: Double -> Int -> Double 
powRec num pow 
    | pow == 1 = num
    | pow <= 0 = error "Invalid power value"
    | otherwise = (*num) $ powRec num (pow - 1)  

powIter:: Double -> Int -> Double
powIter num pow
    | pow <= 0 = error "Invalid power value"
    | otherwise = helper pow
    where 
        helper:: Int -> Double 
        helper 1 = num 
        helper power = (*num) $ helper (power - 1)
            