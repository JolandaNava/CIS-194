module Week4.Four where

import Data.Bits as Bits
import Data.List

---------- ex 1 ----------

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\n -> if (even n) then (div n 2) else (3 * n + 1))

---------- ex 2 ----------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
     deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertBalanced Leaf
    where
        height :: Tree a -> Integer
        height Leaf           = 0
        height (Node h _ _ _) = h

        insertBalanced :: a -> Tree a -> Tree a
        insertBalanced e Leaf = Node 0 Leaf e Leaf
        insertBalanced e t@(Node _ l _ r) =
            case compare (height l) (height r) of
                LT -> instertLeft e t
                GT -> insertRight e t
                EQ ->
                    let inLeft = instertLeft e t
                        inRight = insertRight e t
                    in case compare (height inLeft) (height inRight) of
                        GT -> inRight
                        _  -> inLeft

        instertLeft :: a -> Tree a -> Tree a
        instertLeft e t@(Node h l x r) =
            let addedLeft = insertBalanced e l
                newHeight = (+1) $ max (height r) (height addedLeft)
            in Node newHeight addedLeft x r

        insertRight :: a -> Tree a -> Tree a
        insertRight e t@(Node h l x r) =
            let addedRight = insertBalanced e r
                newHeight = (+1) $ max (height l) (height addedRight)
            in Node newHeight l x addedRight


---------- ex 3.1 ----------

xor' :: [Bool] -> Bool
xor' = foldr Bits.xor False

---------- ex 3.2 ----------

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a -> (++) [f a] ) []

---------- ex 3.3 ----------
-- with help from https://wiki.haskell.org/Foldl_as_foldr
-- and http://blog.shaynefletcher.org/2015/02/fold-left-via-fold-right.html
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\a b x -> b $ f x a) id xs base

---------- ex 4 ----------

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map ((+1) . (*2)) . makeList

makeList  :: Integer -> [Integer]
makeList n = ls \\ [ x | i <- ls, j <- ls, i <= j, x <- [i+j+2*i*j], n > x]
    where ls = [1 .. n]
