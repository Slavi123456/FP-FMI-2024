import Data.Char
import Data.List
main::IO()
main = do
    
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 1) 2 == 3
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 2) 2 == 9
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 3) 2 == 16
    print $ (switchSum (\x -> x + 1) (\x -> x * 2) 4) 2 == 30

switchSum:: (Int -> Int) -> (Int->Int) -> Int -> (Int -> Int)
switchSum _ _ 1 = (\ x -> f x)
switchSum _ _ 2 = (\x -> g $ f x)
--switchSum f g n = (\ x -> f x + g $ f x  )

-- трябва ли да се мъча да правя задачите в един ред или мога да ползвам helper като например на тази
-- да направя списък с големина n където се редуват двете функции и просто 
-- null xs = x
-- x:xs =  recursive xs $ x num 