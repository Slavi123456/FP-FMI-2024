main::IO()
main = do
    -- you may get slightly different results eg. -1.047619047619100 on test 4 <- not a problem
    print $ calcSeriesSum 1 0 == -2.0 -- x = 1, n = 0
    print $ calcSeriesSum 1 1 -- == -0.6666666666666667
    print $ calcSeriesSum 1 2 -- == -1.2000000000000002
    print $ calcSeriesSum 1 3 -- == -1.047619047619048
    print $ calcSeriesSum 1 4 -- == -1.0814814814814817
    print $ calcSeriesSum 1 5 -- == -1.0753246753246755
    print $ calcSeriesSum 1 6 -- == -1.0762718762718764    

calcSeriesSum:: Double -> Double -> Double
calcSeriesSum x n = helper 2 3
    where
        helper::Double -> Double -> Double
        helper idx dev
            | idx > n = (-2)
            | otherwise = (((x**(idx - 1)) * (-2) ** idx) / dev ) + (helper (idx + 1) $ delitel idx 3)

delitel:: Double -> Double -> Double
delitel 1 x = x 
delitel idx x = x * delitel (idx - 1) (x + 2)

-- Description:

-- Define a function that accepts two real numbers and calculates the n-th partial sum from the following sequence: