main::IO()
main = do
    print $ (setupRobots [0, 1] "LR") 3 == [-3, 4]
    print $ (setupRobots [-2, 0, 2] "RLL") 2 == [-2, 0, 0]
    print $ (setupRobots [-2, 0, 2] "RLL") 5 == [-5, -3, 3]
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 1 == [-1,-1,0,2,5,8,9,13,14]
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 3 == [-3,-2,0,1,7,7,10,12,15]
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 5 == [-5,-4,-2,3,5,9,10,12,17]

--------------------------------------------------------------------------------------------------
--Proverki kak raboti koda dali raboti i nqkoi chudeniq
    print $ moveInDirection [(-2, "R"), (0 , "L"), (2, "L")]
    -- print $ map (\y -> snd y == "L") [(-2, "R"), (0 , "L"), (2, "L")]
    -- print $ [x | x <- [(-2, "R"), (0 , "L"), (2, "L")], snd x == "L"]
    print $ listColl  [(-2, "R"), (0 , "L"), (2, "L")]
    --print $ changeDirByLeft (2, "L") [(-2, "R"), (0 , "L"), (2, "L")]
    --print $ snd (-2, "R") == "RC2"
    print $ moveInDirection $ listColl  [(-2, "R"), (0 , "L"), (2, "L")]
    print $ makeTuple [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL"
    print $ moveInDirection $ listColl $ moveInDirection $ listColl  [(-2, "R"), (0 , "L"), (2, "L")]
    print $ doFuncNTimes moveSeuqnce 2 [(-2, "R"), (0 , "L"), (2, "L")]
    
    print $ (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 1

    print $ listInt [(-2, "R"), (0 , "L"), (2, "L")]
    
-- setupRobots :: [Int] -> String -> (Int -> [Int])

--------------------------------------------------------------------------------------------------
--TASKS AND IDEAS
----------------------------------------

--da pravi [((Int, String),(Int, String))] s tezi koito shte kolidirat

--v moveInDirection da ima specialni sluchai za "RC2" i "RC1" == Right Collision First
-- tova izkluchva gornoto 
-- i susto veche kato gi pishesh shte trqbva da zapazvash samo head-a na direction v moveInDirection

-- kato moveInDirection e func-iqta koqto shte se pravi na 1 sec 
-- -> trqbva da se vidi pri  [(-2, "R"), (0 , "L"), (2, "L")] za 1 sec dali shte e [-1,-1, 1]

-- trqbva po hubava proverka za collision zashtoto [(-2, "R"), (0 , "L"), (2, "L")] tretoto e v koliziq a realno ne e
-- da se gleda koi purvo sa v obratnata posoka i togava any za posiciq

--funciqta za generirane na [(Int, String)] ot inputa na zadachata 

--------------------------------------------------------------------------------------------------


--forth one to do
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

--------------------------------------------------------------------------------------------------
--third one to do
-- listColl:: [(Int, String)] -> [Bool]
-- listColl ls = checkCollisions ls

-- checkCollisions:: [(Int, String)] -> [Bool]
-- checkCollisions [] = []
-- checkCollisions ((pos, dir):xs) = helper dir : checkCollisions xs
--     where
--         helper:: String -> Bool
--         helper "R" = (any (\ x -> fst x == (pos + 1)) $ [y | y <- ls, snd y == "L"]) || (any (\ x -> fst x == (pos + 2)) $ [y | y <- ls, snd y == "L"])
--         helper "L" = (any (\ x -> fst x == (pos - 1)) $ [y | y <- ls, snd y == "R"]) || (any (\ x -> fst x == (pos - 2)) $ [y | y <- ls, snd y == "R"]) 
--         helper _ = error "Invalid direction"
    
--------------------------------------------------------------------------------------------------
--second one to do
-- checkCollisions:: [(Int, String)] -> [Bool]
-- checkCollisions [] = []
-- checkCollisions ((pos, dir):xs) = helper dir : checkCollisions xs -- PROBLEM SLED PURVI ELEMENT
--     where
--         helper:: String -> Bool
--         helper "R" = (any (\ x -> fst x == (pos + 1)) xs) || (any (\ x -> fst x == (pos + 2)) xs)
--         helper "L" = (any (\ x -> fst x == (pos - 1)) xs) || (any (\ x -> fst x == (pos - 2)) xs)
--         helper _ = error "Invalid direction"

--------------------------------------------------------------------------------------------------
--first one to do
-- AFTER forth added RC1 LC1 ...
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
--------------------------------------------------------------------------------------------------
--these were with ChatGPT help :D
--fifth one to do
makeTuple :: [Int] -> String -> [(Int, String)]
makeTuple [] _ = []  
makeTuple _ [] = []  
makeTuple (x:xs) (c:cs) = (x, [c]) : makeTuple xs cs

--sixt one to do
-- applyNTimes :: ([(Int, String)] -> [(Int, String)]) -> Int -> [(Int, String)]
-- applyNTimes _ 0 = [] 
-- applyNTimes f n = applyNTimes f (n-1) 

--this one is the one above but little changed
doFuncNTimes :: ([(Int, String)] -> [(Int, String)]) -> Int -> [(Int, String)] -> [(Int, String)]
doFuncNTimes _ 0 xs = xs 
doFuncNTimes f n xs = doFuncNTimes f (n-1) (f xs)
--------------------------------------------------------------------------------------------------
--seventh one to do
moveSeuqnce:: [(Int, String)] -> [(Int, String)]
moveSeuqnce xs =  moveInDirection $ listColl xs
--------------------------------------------------------------------------------------------------
--eight one to do
--need fix 
-- setupRobots :: [Int] -> String -> (Int -> [(Int,String)])--(Int -> [Int])
-- setupRobots xs str = (\ x -> doFuncNTimes moveSeuqnce x (makeTuple xs str))

--AFTER nineth i added listInt
--SOLO DONE
setupRobots :: [Int] -> String ->(Int -> [Int])
setupRobots xs str = (\ x -> listInt $ doFuncNTimes moveSeuqnce x (makeTuple xs str))

--nineth one to do
-- ChatGPT
listInt:: [(Int, String)] -> [Int]
listInt xs = [x | (x, _) <- xs]