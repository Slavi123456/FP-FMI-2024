main::IO()
main = do
    -- print $ warmerAfter [20,21,20,19,18,20,25,24,23,20,26] == [1,5,4,2,1,1,4,3,2,1,0]
    -- print $ warmerAfter [0,10,20,30] == [1,1,1,0]
    -- print $ warmerAfter [21,22,23] ==  [1,1,0]
    -- print $ warmerAfter [23,24,25,21,19,23,26,23] == [1,1,4,2,1,1,0,0]
    -- print $ (setupRobots [0, 1] "LR") 3 == [-3, 4]
    -- print $ (setupRobots [-2, 0, 2] "RLL") 2 == [-2, 0, 0]
    -- print $ (setupRobots [-2, 0, 2] "RLL") 5 == [-5, -3, 3]
    -- print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 1 == [-1,-1,0,2,5,8,9,13,14]
    -- print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 3 == [-3,-2,0,1,7,7,10,12,15]
    -- print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 5 == [-5,-4,-2,3,5,9,10,12,17]

--purva zadacha
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

--vtora zadacha

listColl:: [(Int, String)] -> [(Int, String)]
listColl ls = checkCollisions ls
    where
        checkCollisions:: [(Int, String)] -> [(Int, String)]
        checkCollisions [] = []
        checkCollisions (t@(pos, dir):xs) = helper dir : checkCollisions xs
            where
                helper:: String -> (Int, String)
                helper "R" = changeDirByRight t ls
                helper "L" = changeDirByLeft t ls
                helper _ = error "Invalid direction"

changeDirByRight:: (Int, String) -> [(Int, String)] -> (Int, String)
changeDirByRight (pos, dir) ls
    | (any (\ x -> fst x == (pos + 1)) $ [y | y <- ls, snd y == "L"]) = (pos, "RC1")
    | (any (\ x -> fst x == (pos + 2)) $ [y | y <- ls, snd y == "L"]) = (pos, "RC2")
    | otherwise = (pos, dir)

changeDirByLeft:: (Int, String) -> [(Int, String)] -> (Int, String)
changeDirByLeft (pos, dir) ls
    | (any (\ x -> fst x == (pos - 1)) $ [y | y <- ls, snd y == "R"]) = (pos, "LC1")
    | (any (\ x -> fst x == (pos - 2)) $ [y | y <- ls, snd y == "R"]) = (pos, "LC2")
    | otherwise = (pos, dir)

moveInDirection:: [(Int, String)] -> [(Int, String)]
moveInDirection [] = []
moveInDirection ((pos, dir):xs)
    | dir == "R" = ((pos + 1), dir) : moveInDirection xs
    | dir == "L" = ((pos - 1), dir) : moveInDirection xs
    | dir == "RC1" =  (pos, "L") : moveInDirection xs
    | dir == "RC2" =  ((pos + 1), "L") : moveInDirection xs
    | dir == "LC1" =  (pos, "R") : moveInDirection xs
    | dir == "LC2" =  ((pos - 1), "R") : moveInDirection xs
    | otherwise = error "Invalid direction in moveInDirection"

makeTuple :: [Int] -> String -> [(Int, String)]
makeTuple [] _ = []  
makeTuple _ [] = []  
makeTuple (x:xs) (c:cs) = (x, [c]) : makeTuple xs cs

doFuncNTimes :: ([(Int, String)] -> [(Int, String)]) -> Int -> [(Int, String)] -> [(Int, String)]
doFuncNTimes _ 0 xs = xs 
doFuncNTimes f n xs = doFuncNTimes f (n-1) (f xs)

moveSeuqnce:: [(Int, String)] -> [(Int, String)]
moveSeuqnce xs =  moveInDirection $ listColl xs

setupRobots :: [Int] -> String ->(Int -> [Int])
setupRobots xs str = (\ x -> listInt $ doFuncNTimes moveSeuqnce x (makeTuple xs str))

listInt:: [(Int, String)] -> [Int]
listInt xs = [x | (x, _) <- xs]