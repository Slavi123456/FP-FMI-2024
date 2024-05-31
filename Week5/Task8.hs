import Data.Char
import Data.List
main::IO()
main = do
    print $ (repeater "I love Haskell") 3 " " -- == "I love Haskell I love Haskell I love Haskell"
    print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"
    --print $ concat ["I love Haskell", "Quack"]
repeater:: String -> (Int -> String -> String)
repeater str = (\ x gl -> helper gl (concat [str, gl]) 1 x [])
    where
        helper:: String -> String -> Int -> Int -> String -> String
        helper glue mem count max res    
            | count == max = concat [res,str]
            | otherwise = helper glue mem (count + 1) max (concat [res,mem])


-- Дефинирайте функция repeater str, която получава като аргумент символен низ и връща анонимна 
-- функция на два аргумента - count и glue (число и низ). Оценката на обръщението към върнатата 
-- функция е низ, който се получава чрез count-кратно повтаряне на низа str, при което между 
-- всеки две съседни повторения на str стои низът glue.

-- Acceptance criteria:

-- All tests pass.
-- Add one new test case. Place a comment after it with the words my test.