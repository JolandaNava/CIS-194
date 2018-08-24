module Week11.SExpr where

import Control.Applicative
import Data.Char
import Week11.AParser

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p $ zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------
-- definitions
type Ident = String

data Atom = N Integer | I Ident
  deriving Show

data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
------------------------------------------------------------

parseSExpr :: Parser SExpr
parseSExpr = atomExpr <|> combExpr

combExpr :: Parser SExpr
combExpr = Comb <$> (open *> oneOrMore parseSExpr) <* close
  where
    open = char '(' *> spaces
    close = char ')' <* spaces

atomExpr :: Parser SExpr
atomExpr = A <$> (atomIdent <|> atomInt) <* spaces
  where
    atomIdent =  I <$> ident
    atomInt = N <$> posInt
