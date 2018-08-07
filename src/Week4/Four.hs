module Week4.Four where

import Data.Bits as Bits

---------- ex 1 ----------

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map (flip (-) 2) . filter (even)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\n -> if (even n) then (div n 2) else (3 * n + 1))

---------- ex 2 ----------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
     deriving (Show, Eq)

---------- ex 3.1 ----------

xor' :: [Bool] -> Bool
xor' = foldr Bits.xor False

---------- ex 3.2 ----------

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a -> (++) [f a] ) []

---------- ex 3.3 ----------

-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr ...

---------- ex 4 ----------

