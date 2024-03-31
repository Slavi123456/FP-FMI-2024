main::IO()
main = do 
    print $ sqAvg 5 0 == 12.5
    print $ sqAvg 10 13 == 134.5
    print $ sqAvg 6 8 == 50
    --my test 
sqAvg::Int -> Int -> Double
sqAvg x y = (/2) $ fromIntegral $ x^2 + y^2