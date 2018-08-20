{-# LANGUAGE FlexibleInstances #-}
module Week6.Fibonacci where


---------- ex 1 ----------
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

---------- ex 2 ----------
fibs2 :: [Integer]
fibs2 = [0, 1] ++ fibh 0 1
    where
        fibh :: Integer -> Integer -> [Integer]
        fibh x y = (x+y) : (fibh y (x+y))

---------- ex 3 ----------
data Stream a =
    Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x sx) = x : streamToList sx

instance Show a => Show (Stream a) where
    show = show . take 40 . streamToList

---------- ex 4 ----------
streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream a sa) = Stream (f a) (streamMap f sa)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x $ streamFromSeed f (f x)

---------- ex 5 ----------
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x sx) sy = Stream x (interleaveStreams sy sx)

ruler :: Stream Integer
ruler = recursivelyInterleave streamOfStreams
    where
        streamOfStreams :: Stream (Stream Integer)
        streamOfStreams = streamMap streamRepeat nats

        recursivelyInterleave :: Stream (Stream Integer) -> Stream Integer
        recursivelyInterleave (Stream s ss) = interleaveStreams s $ recursivelyInterleave ss

---------- ex 6 ----------
x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
    fromInteger n = Stream n $ streamRepeat 0
    negate = streamMap negate
    (+) (Stream x sx ) (Stream y sy) = Stream (x+y) $ sx + sy
    (*) (Stream x sx ) yy@(Stream y sy) = Stream (x*y) $ streamMap (*x) sy + sx * yy

instance Fractional (Stream Integer) where
    (/) (Stream x sx ) (Stream y sy) = q
        where
            q = Stream (x `div` y) $ streamMap (`div` y) (sx - (q * sy))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

---------- ex 7 ----------
type Matrix = ((Integer, Integer), (Integer, Integer))

instance Num Matrix where
    (*) ((a11,a12), (a21,a22)) ((b11,b12),(b21,b22)) = ((c11,c12),(c21,c22))
        where
            c11 = a11*b11 + a12*b21
            c12 = a11*b12 + a12*b22
            c21 = a21*b11 + a22*b21
            c22 = a21*b12 + a22*b22

f :: Matrix
f = ((1,1),(1,0))

fib4 :: Integer -> Integer
-- fib4 0 = 0
-- I can't specify the 0th case if I want to eta reduce fib4
fib4 = snd . fst . (^) f
