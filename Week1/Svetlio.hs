main::IO()
main = do
    print(myMin 5 6)

myMin:: Int -> Int -> Int
myMin x y = if x > y then y else x