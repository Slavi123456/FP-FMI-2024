main::IO()
main = do
    print $ growingPlant 5 2 5 == 1
    print $ growingPlant 5 2 6 == 2
    print $ growingPlant 10 9 4 == 1
    print $ growingPlant 100 10 910 == 10 -- upSpeed=100, downSpeed=10, desiredHeight=910
    print $ growingPlant 1 3 7 == 1 --output needs to be -2
    --my test
    
    print "Snail task (Task10) just moved the first parameter last"
    print $ growingPlant 2 1 3 == 2
    print $ growingPlant 3 1 10 == 5
    print $ growingPlant 3 2 10 == 8
    print $ growingPlant 20 5 100 == 7
    print $ growingPlant 10 3 5 == 1
    print $ growingPlant 9 3 7 == 1
    --my test

growingPlant:: Int -> Int -> Int -> Int
growingPlant up down goal
    | up < 0 || down < 0 || goal < 0 = -1 --(-1) for invalid input
    | up >= goal = 1
    | up < goal && up < down = -2 --(-2) for never getting the goals
    | otherwise = 1 + growingPlant up down (goal - up + down)
    --otherwise = 1 + dayToReachGoal up down (goal - up + down)

dayToReachGoal:: Int -> Int -> Int -> Int -> Int -> Int -> Int
dayToReachGoal up down goal currenHeight counterDayOrNight day
            | goal <= currenHeight = day
            | mod counterDayOrNight 2 == 0 = dayToReachGoal up down goal (currenHeight + up) (counterDayOrNight + 1) day
            | mod counterDayOrNight 2 /= 0 = dayToReachGoal up down goal (currenHeight - down) (counterDayOrNight + 1) (day + 1)