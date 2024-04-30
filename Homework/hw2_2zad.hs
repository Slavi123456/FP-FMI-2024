main::IO()
main = do
    -- (setupRobots [0, 1] "LR") 3 == [-3, 4]
    -- (setupRobots [-2, 0, 2] "RLL") 2 == [-2, 0, 0]
    -- (setupRobots [-2, 0, 2] "RLL") 5 == [-5, -3, 3]
    -- (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 1 == [-1,-1,0,2,5,8,9,13,14]
    -- (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 3 == [-3,-2,0,1,7,7,10,12,15]
    -- (setupRobots [-2,0,1,3,4,7,10,12,15] "RLLLRRLRL") 5 == [-5,-4,-2,3,5,9,10,12,17]
    print $ moveInDirection [(-2, "R"), (0 , "L"), (2, "L")]
    print $ checkCollisions [(-2, "R"), (0 , "L"), (2, "L")]

-- setupRobots :: [Int] -> String -> (Int -> [Int])

checkCollisions:: [(Int, String)] -> [Bool]
checkCollisions [] = []
checkCollisions ((pos, dir):xs) = helper dir : checkCollisions xs
    where
        helper:: String -> Bool
        helper "R" = (any (\ x -> fst x == (pos + 1)) xs) || (any (\ x -> fst x == (pos + 2)) xs)
        helper "L" = (any (\ x -> fst x == (pos - 1)) xs) || (any (\ x -> fst x == (pos - 2)) xs)
        helper _ = error "Invalid direction"

moveInDirection:: [(Int, String)] -> [Int]
moveInDirection [] = []
moveInDirection ((pos, dir):xs)
    | dir == "R" = (pos + 1) : moveInDirection xs
    | dir == "L" = (pos - 1) : moveInDirection xs
    | otherwise = error "Invalid direction"