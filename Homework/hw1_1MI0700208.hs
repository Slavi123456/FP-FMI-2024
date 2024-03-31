
--Първа задача 
numStepCombinations :: Int -> Int 
numStepCombinations steps 
    | steps == 1 = 1 
    | steps == 2 = 2
    | otherwise = numStepCombinations (steps - 1) + numStepCombinations (steps - 2)

--Втора задача
maxPersistenceMinSum :: Int -> Int -> Int
maxPersistenceMinSum num num2 = help (num + 1) num (sumDigigits num)
    where 
        help:: Int -> Int -> Int -> Int
        help n max sumMax
            | n == num2 = max
            | sustainableNumSteps n > sustainableNumSteps max = help (n + 1) n (sumDigigits n)
            | sustainableNumSteps n == sustainableNumSteps max && sumDigigits n < sumDigigits max = help (n + 1) n (sumDigigits n)
            | otherwise = help (n + 1) max (sumDigigits max)

sustainableNumSteps:: Int -> Int
sustainableNumSteps num
    | productDigits num < 10 = 1
    | otherwise = 1 + sustainableNumSteps (productDigits num) 
    where
        productDigits:: Int -> Int 
        productDigits n
            | n < 10 = n
            | mod n 10 == 0 = productDigits $ div n 10
            | otherwise = mod n 10 * (productDigits (div n 10)) 

sumDigigits:: Int -> Int 
sumDigigits num
    | num < 10 = num
    | otherwise = mod num 10 + (sumDigigits (div num 10))