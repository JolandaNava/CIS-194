module Week4.Four where

---------- ex 1 ----------

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map (flip (-) 2) . filter (even)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\n -> if (even n) then (div n 2) else (3 * n + 1))

---------- ex 2 ----------

