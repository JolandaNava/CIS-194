module Homework3 where

import Data.List

---------- ex 1 ----------

skips :: [a] -> [[a]]
skips l =  map (pickNth l) [1..(length l)]
    where
        pickNth :: [a] -> Int -> [a]
        pickNth l n =
            case (drop (n-1) l) of
                x:xs -> x:(pickNth xs n)
                []   -> []

---------- ex 2 ----------

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) =
 if x<y && z<y
 then y:(localMaxima (y:z:xs))
 else (localMaxima (y:z:xs))
localMaxima _ = []

---------- ex 3 ----------

histogram :: [Integer] -> String
histogram l = hist l ++ "==========\n0123456789\n\n"
    where
        hist l@(x:xs)  = (hist $ foldl (flip id) l $ map delete [0..9]) ++ ((?) '*' ' ' <$> map (flip elem l) [0..9]) ++ "\n"
        hist [] = ""




---------- choice helper ----------
(?) :: a -> a -> Bool -> a
(?) t f b = if b then t else f

---------- breakdown of histogram ----------
isThere :: [Integer] -> [Bool]
isThere l =  map (flip elem l) [0..9]

toString :: [Bool] -> String
toString lb = (?) '*' ' ' <$> lb

nextRow :: [Integer] -> [Integer]
nextRow l = foldl (flip id) l $ listOfFunctions

listOfFunctions :: [([Integer] -> [Integer])]
listOfFunctions = map delete [0..9]

simpleHist :: [Integer] -> String
simpleHist l@(x:xs) = (simpleHist $ nextRow l) ++ (toString $ isThere l) ++ "\n"

