main:: IO()
main = do
    print $ finalGrade 3 4 4 4.25 4.50 3.75 4.25 5 4.25 == 4.34
    print $ finalGrade 6 6 6 4.50 5 4.50 4.75 5 4.75    == 4.95
    print $ finalGrade 6 0 4 6 6 5 4.75 6 4.75          == 5.14
    print $ finalGrade 4.25 0 3 2 0 0 0 0 0             == 2
    print $ finalGrade 5.50 6 6 6 5.50 5.25 4 5.50 4    == 5.05
    print $ finalGrade 6 6 6 5.50 5.50 4 5 5.50 5       == 5.25
    print $ finalGrade 6 6 6 5.25 6 4 4 5.63 3.50       == 4.84
    print $ finalGrade 6 6 6 4.50 5 4.50 4.75 5 4.75
    print $ finalGrade 6 0 4 6 6 5 4.75 6 4.75
    print $ finalGrade 4.25 0 3 2 0 0 0 0 0
    print $ finalGrade 5.50 6 6 6 5.50 5.25 4 5.50 4    
    print $ finalGrade 6 6 6 5.50 5.50 4 5 5.50 5       
    print $ finalGrade 6 6 6 5.25 6 4 4 5.63 3.50

finalGrade:: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
finalGrade d1 d2 d3 kz1 kz2 kt1 kt2 iz it
    |roundTwoDig (outPutTK d1 d2 d3 kz1 kz2 kt1 kt2 /2 + it / 4 + iz / 4) < 2 = 2
    |averageOfTwo kt1 kt2 >= 4.50 
        && kt1 >= 4.00 
        && kt2 >= 4.00 
        && averageOfTwo kz1 kz2 >= 4.50 
        && kz1 >= 4.00 
        && kz2 >= 4.0 = roundTwoDig (outPutTK d1 d2 d3 kz1 kz2 kt1 kt2 /2 + averageOfTwo kt1 kt2 / 4 + averageOfTwo kz1 kz2 / 4)
    |averageOfTwo kt1 kt2 >= 4.50 
        && kt1 >= 4.00 
        && kt2 >= 4.0 = roundTwoDig (outPutTK d1 d2 d3 kz1 kz2 kt1 kt2 /2 + averageOfTwo kt1 kt2 / 4 + iz / 4)
    |averageOfTwo kz1 kz2 >= 4.50 
        && kz1 >= 4.00 
        && kz2 >= 4.0 = roundTwoDig (outPutTK d1 d2 d3 kz1 kz2 kt1 kt2 /2 + it / 4 + averageOfTwo kz1 kz2 / 4)
    |otherwise = roundTwoDig (outPutTK d1 d2 d3 kz1 kz2 kt1 kt2 /2 + it / 4 + iz / 4)

roundTwoDig:: Double -> Double
roundTwoDig x = (/100) $ fromIntegral $ round $ x*100

outPutTK:: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
outPutTK d1 d2 d3 kz1 kz2 kt1 kt2 = (/8) $ 2*averageOfThree d1 d2 d3 + 3* (averageOfTwo kz1 kz2 + averageOfTwo kt1 kt2 )

averageOfThree:: Double -> Double -> Double -> Double
averageOfThree x y z = (/ 3) $ x + y + z

averageOfTwo:: Double -> Double -> Double
averageOfTwo x y = (/2) $ x + y



--O = 1/2 TK + 1/4 IT + 1/4 IZ
--TK = 1/4 D + 3/8 KT + 3/8 KZ

--D is the average grade from 3 homeworks;
--KT is the average grade from 2 theory exams;
--KZ is the average grade from 2 practice exams;
--IT is the grade from the theory final;
--IZ is the grade from the practice final.