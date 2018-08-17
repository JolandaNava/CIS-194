{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Week5.Calc where

import Week5.ExprT (ExprT (..))
import Week5.Parser (parseExp)
import qualified Week5.StackVM as VM (Program, StackExp (..), stackVM)

import Control.Applicative (liftA2)

---------- ex 1 ----------
eval :: ExprT -> Integer
eval (Lit n)   = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

---------- ex 2 ----------
evalStr :: String -> Maybe Integer
evalStr str = eval <$> parseExp Lit Add Mul str

---------- ex 3 ---------
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

---------- ex 4 ----------
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (<0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer
    deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax $ max x y
    mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer
    deriving (Eq, Show)

instance Expr Mod7 where
    lit = Mod7 . (`mod` 7)
    add (Mod7 x) (Mod7 y) = lit $ x + y
    mul (Mod7 x) (Mod7 y) = lit $ x * y

---------- ex 5 ----------
instance Expr VM.Program where
    lit x = [VM.PushI x]
    add x y = concat [x, y, [VM.Add]]
    mul x y = concat [x, y, [VM.Mul]]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul
