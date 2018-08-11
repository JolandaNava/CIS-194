module Week5.Calc where

import Week5.ExprT (ExprT (..))
import Week5.Parser (parseExp)

---------- ex 1 ----------
eval :: ExprT -> Integer
eval (Lit n)       = n
eval (Add ex1 ex2) = eval ex1 + eval ex2
eval (Mul ex1 ex2) = eval ex1 * eval ex2

---------- ex 2 ----------

evalStr :: String -> Maybe Integer
evalStr str = eval <$> parseExp Lit Add Mul str

---------- ex 3 ----------
