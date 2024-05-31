import Data.Char
import Data.List
main::IO()
main = do
    
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 1) 2 == 3
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 2) 2 == 9
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 3) 2 == 16
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 4) 2 == 30

switchSum:: (Int -> Int) -> (Int->Int) -> Int -> (Int -> Int)
switchSum f _ 1 = (\ x -> f x)
switchSum f g n = (\ x -> helper 0 x 0) 
    where
        helper:: Int -> Int -> Int -> Int
        helper num mem res
            | num == n = res
            | mod num 2 == 0 = helper (num + 1) (f mem) (res + f mem)
            | otherwise = helper (num + 1) (g mem) (res + g mem)