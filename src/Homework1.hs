module Homework1 where

-- 1
toDigits :: Integer -> [Integer]
toDigits num =
  helper num []
 where
  helper :: Integer -> [Integer] -> [Integer]
  helper n l =
   if n < 1
   then l
   else helper (div n 10) ((mod n 10):l)

toDigitsRev :: Integer -> [Integer]
toDigitsRev num  =
  reverse (toDigits num)

-- 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list =
    map altDouble isEven
   where
    isEven :: [(Integer, Int)]
    isEven =
     let len = length list
     in zip list (reverse [1 .. len])

    altDouble :: (Integer, Int) -> Integer
    altDouble (n, i) =
     if (mod i 2 == 0)
     then (n * 2)
     else n

-- 3
sumDigits :: [Integer] -> Integer
sumDigits list = foldr (+) 0 $ concat $ map toDigits list

-- 4
validate :: Integer -> Bool
validate num =
 (0 ==) $ (\n -> mod n 10) $ sumDigits $ doubleEveryOther $ toDigits num

-- 5
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi num one two three =
    helper num one two three []
   where
    helper :: Integer -> Peg -> Peg -> Peg -> [Move] -> [Move]
    helper n a b c moves =
     if n == 0
     then []
     else (helper (n - 1) a c b [] ) ++ [(a, b)] ++ (helper (n-1) c b a [] )
