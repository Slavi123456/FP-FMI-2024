main:: IO()
main = do
    
    print "Hello world"
    print "Min functions"
    print $ minIf 15 60 == 15
    print $ minIf 60 15 == 15
    print $ minIf (-60) 15 == 15

    print $ minGuard 15 60 == 15
    print $ minGuard 60 15 == 15
    
    print $ minBuiltIn 60 15 == 15
    
    print "Digit functions"
    print $ lastDigit 154 == 4
    print $ removeLastDigit 154 == 15  

    print "Devision"
    print $ quotientWhole 64 2 == 32
    print $ divWhole 154 17 == 9.058823529411764
    print $ divReal 154.451 10.01 == 15.42967032967033
    print $ quotientReal 154.21 17.17 == 8
    
    print "Avr and Round"
    print $ avgWhole 5 1542 2 == 773.5
    print $ roundTwoDig 3.1413465345321 == 3.14
    print $ roundTwoDigButWithMagic 3.1413465345321 == 3.14

minIf:: Int -> Int -> Int
minIf x y = if x > y then y else x

minGuard:: Int -> Int -> Int
minGuard x y 
    | x > y = y
    | otherwise = x

minBuiltIn:: Int -> Int -> Int 
minBuiltIn x y = min x y

lastDigit :: Int -> Int
lastDigit = mod 10

removeLastDigit:: Int -> Int
removeLastDigit x = div x 10

quotientWhole:: Int -> Int -> Int
quotientWhole x y = div x y

divWhole:: Int -> Int -> Double
divWhole x y = fromIntegral x / fromIntegral y

divReal:: Double -> Double -> Double
divReal x y = (/) x y

quotientReal:: Double -> Double -> Int
quotientReal x y = floor $ (/) x y

avgWhole:: Int -> Int -> Int -> Double
--avgWhole x y z =  (/) $ sumIntToDouble x y (fromIntegral 2)
avgWhole x y z =  (sumIntToDouble x y) / (fromIntegral z)

sumIntToDouble:: Int -> Int -> Double
sumIntToDouble x y = fromIntegral $ x + y

roundTwoDig:: Double -> Double
roundTwoDig x = fromIntegral(floor (x*100))/100

productDouble:: Double -> Double -> Double
productDouble x y = x * y 

roundTwoDigButWithMagic:: Double -> Double
--roundTwoDigButWithMagic = roundTwoDig
--roundTwoDigButWithMagic x = (/100) $ fromIntegral $ floor $ (*) x 100 
--roundTwoDigButWithMagic x = (/100) $ fromIntegral $ floor $ x*100
roundTwoDigButWithMagic = (/100) . fromIntegral . floor .  (*100)

--We don't need brackets because (/) x y is equivalent with x / y and (/) is taken like a function with two arguments  
