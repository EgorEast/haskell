module Egor where

x = [1, 2, 3]

a = 's'

-- function
square :: Integer -> Integer
square x = x * x

-- function
pow :: (Integer, Integer) -> Integer
pow (x, y) = x ^ y

i'm = 2

helloWorld :: String
helloWorld = "Hello world"

-- function
signum :: Integer -> Integer
-- условия только внутри функций
-- signum x =
--   if (x > 0)
--     then 1
--     else
--       if x < 0
--         then -1
--         else 0

-- signum x = if isPositive x then 1 
-- else if x < 0 then -1 
-- else 0 

signum x
  | isPositive x = 1
  | x < 0 = -1
  | otherwise = 0


isPositive :: Integer -> Bool
-- isPositive x = if x > 0 then True else False
isPositive x = x > 0


add :: Integer -> Integer -> Integer
add x y = x + y

increment :: Integer -> Integer
increment = add 1