module Week6.Fibonacci where


---------- ex 1 ----------
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

---------- ex 2 ----------
-- fibs2 :: [Integer]
-- fibs2 = map fib' [0..1]
--     where
--         fib' :: Integer -> [(Integer,Integer)] -> Integer
--         fib' 0 _ = 0
--         fib' 1 _ = 1
--         fib' n xs =

