import Data.Char
import Data.List
main:: IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0


--fix task3.hs
--getAreas xs = map area xs
--maxArea xs = head $ [x | (x,y) <- zip xs getAreas xs, y == maximum $ getAreas xs]


-- area::(Num a) => Shape a -> Double
-- area (Rectangle x y) = x * y
-- area (Circle r) = pi * r * r
-- area (Triangle x y z) = sqrt ((div (x + y + z) 2) * ((div (x + y + z) 2) - x) * ((div (x + y + z) 2) - y) * ((div (x + y + z) 2) - z))
-- area (Cylinder r h) = 2 *pi * r * h + 2* pi *r *r