import Data.Char
import Data.List
main::IO()
main = do
    print $ (pairCompose [(+1), (+2)]) 1 == 5 -- ((1 + 2) + 1) + 1
    print $ (pairCompose [(+1), (+2), (+3)]) 1 == 8


--Type Func = (Int -> Int)

pairCompose:: [Int -> Int] -> Int -> Int
pairCompose [] x = x
pairCompose [f] x = f x
pairCompose xs x =  pairCompose (drop 2 xs) x + ((xs!!0) $ (xs!!1) x)