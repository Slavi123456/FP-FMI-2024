import Data.Char
import Data.List
main:: IO()
main = do
    let pi = 3.14
    print $ perimeter (Circle 5) == 31.41592653589793
    print $ perimeter (Rectangle 2.5 4.5) == 14
    print $ perimeter (Rectangle 5.5 20.6) == 52.2
    print $ perimeter (Triangle 5.3 3.9 4.89) == 14.09
    print $ perimeter (Cylinder 2.5 10) == 30

    -- print $ area (Circle 5) == 78.53981633974483
    -- print $ area (Rectangle 2.5 4.5) == 11.25
    -- print $ area (Rectangle 5.5 20.6) == 113.30000000000001
    -- print $ area (Triangle 5.3 3.9 4.89) == 9.127927385194024
    -- print $ area (Cylinder 20 30) == 6283.185307179587


data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Show, Eq, Read, Ord)

--How to make it more generelized for Int and Double
perimeter:: (Num a)  => Shape a -> a
perimeter (Rectangle x y) = (2 * (x + y))
perimeter (Circle r) =  (2 * 3 * r) --it's 3 on purpose
perimeter (Triangle x y z) = (x + y + z)
perimeter (Cylinder r h) = (4 * r + 2* h)

-- perimeter:: (Num a) => Shape a -> Double
-- perimeter (Rectangle x y) = 2 * (x + y)
-- perimeter (Circle r) = 2 * pi * r
-- perimeter (Triangle x y z) = x + y + z
-- perimeter (Cylinder r h) = 4r + 2h

-- area::(Num a) => Shape a -> Double
-- area (Rectangle x y) = x * y
-- area (Circle r) = pi * r * r
-- area (Triangle x y z) = sqrt ((div (x + y + z) 2) * ((div (x + y + z) 2) - x) * ((div (x + y + z) 2) - y) * ((div (x + y + z) 2) - z))
-- area (Cylinder r h) = 2 *pi * r * h + 2* pi *r *r

-- Description:

-- By using the Shape data type, define the following functions:

-- perimeter;
-- area.
-- Recap:

-- Perimeter of a cylinder: 4r + 2h Area of a cylinder: 2pirh + 2pirr.

-- Acceptance criteria:

-- Add one new test case. Place a comment after it with the words my test.
-- All tests pass