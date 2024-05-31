import Data.Char
import Data.List
main::IO()
main = do
    print $ dotProduct (1, 2, 3) (7, 4, 1) == 18
    print $ dotProduct (5, 2, 159) (0, -1, -2) == (-320)

    print $ crossProduct (1, 2, 3) (7, 4, 1) == (-10, 20, -10)
    print $ crossProduct (5, 2, 159) (0, -1, -2) == (155, 10, -5)

    print $ magnitude (1, 2, 3) == 3.7416573867739413
    print $ magnitude (7, 4, 1) == 8.12403840463596
    print $ magnitude (-10, 20, -10) == 24.49489742783178
    print $ magnitude (5, 2, 159) == 159.0911688309568
    print $ magnitude (0, -1, -2) == 2.23606797749979
    print $ magnitude (155, 10, -5) == 155.40270267920053
    --print $ (2.1^2 + 2.1^2) ** 0.5


type Vector a = (a, a, a)

dotProduct:: (Num a) => Vector a-> Vector a -> a
dotProduct (x,y,z) (a,b,c) = x * a + y * b + z * c

crossProduct::(Num a) => Vector a-> Vector a-> Vector a
crossProduct (x,y,z) (a,b,c) = (determinant (y,z) (b,c),determinant (z,x) (c,a),determinant (x,y) (a,b))
    where
        determinant:: (Num a) => (a,a) -> (a,a) -> a
        determinant (x,y) (a,b) = x* b - y * a

magnitude:: (Integral a) => Vector a -> Double
magnitude (x,y,z) = sqrt $ fromIntegral $ (x^2 + y^2 + z^2)

-- Description:

-- For the Vector data type we defined in class, define the following functions:

-- dotProduct: scalar product;
-- crossProduct: vector product;
-- magnitude: length of a vector.
-- Acceptance criteria:

-- All tests pass.
-- Typeclasses are used.
-- Add one new test case. Place a comment after it with the words my test.