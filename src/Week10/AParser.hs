module Week10.AParser where

import Control.Applicative
import Data.Char

------------------------------------------------------------
-- Provided code
------------------------------------------------------------
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Homework code begins here
------------------------------------------------------------

---------- ex 1 ----------
first :: (a -> b) -> (a,c) -> (b,c)
first f (a,c) = (f a, c)

instance Functor Parser where
  fmap f parser = Parser $ fmap (first f) . runParser parser

---------- ex 2 ----------
instance Applicative Parser where
  pure a = Parser $ \_ -> Just (a, "") -- ???

  p1 <*> p2 = Parser f
    where
      f1 = runParser p1
      f2 = runParser p2
      f str =
        case f1 str of
          Nothing      -> Nothing
          Just (g, xs) -> case f2 xs of
                            Nothing      -> Nothing
                            Just (a, ys) -> Just (g a, ys)

---------- ex 3 ----------
abParser :: Parser (Char,Char)
abParser = (,) <$> (char 'a') <*> (char 'b')
