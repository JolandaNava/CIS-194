module Week1.Homework1 where

-- 1
toDigits :: Integer -> [Integer]
toDigits num =
  helper num []
 where
  helper :: Integer -> [Integer] -> [Integer]
  helper n l =
   if n < 1
   then l
   else helper a $ b:l
    where
      (a,b) = divMod n 10

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list =
    map altDouble isEven
   where
    len = length list

    isEven :: [(Integer, Int)]
    isEven = zip list [len, len-1 .. 1]

    altDouble :: (Integer, Int) -> Integer
    altDouble (n, i) =
     if (mod i 2 == 0)
     then (n * 2)
     else n

-- 3
sumDigits :: [Integer] -> Integer
sumDigits = sum . (concatMap toDigits)

-- 4
validate :: Integer -> Bool
validate = (0 ==) . (flip mod 10) . sumDigits . doubleEveryOther . toDigits

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
