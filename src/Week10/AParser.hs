module Week10.AParser where

import Control.Applicative
import Control.Monad
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
  pure a = Parser $ const $ Just (a, "")

  (Parser f1) <*> (Parser f2) = Parser $ f
    where
      f str = join $ (\(g,xs) -> (first g) <$> f2 xs) <$> f1 str

---------- ex 3 ----------
abParser :: Parser (Char,Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair = toList <$> intSpace <*> posInt
  where
    intSpace :: Parser Integer
    intSpace = posInt <* char ' '

    toList = \x y -> [x,y]

---------- ex 4 ----------
instance Alternative Parser where
  empty = Parser $ const Nothing

  (<|>) (Parser f1) (Parser f2) = Parser $ \s -> f1 s <|> f2 s

---------- ex 5 ----------
intOrUppercase :: Parser ()
intOrUppercase = void (satisfy isUpper) <|> void posInt
