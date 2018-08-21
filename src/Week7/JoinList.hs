{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Week7.JoinList where

import Data.Monoid
import Week7.Buffer
import Week7.Editor
import Week7.Scrabble
import Week7.Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

---------- ex 1 ----------
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl = jl
(+++) jl Empty = jl
(+++) jl1 jl2  = Append (tag jl1 <> tag jl2) jl1 jl2

---------- ex 2.1 ----------
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ l r) =
    case compare (i+1) lSize of
        GT -> indexJ (i-lSize) r
        _  -> indexJ i l
    where
        lSize = getSize $ size $ tag l

---------- ex 2.2 ----------
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 jl = jl
dropJ _ (Single _ _ ) = Empty
dropJ n (Append m l r) =
    case compare n lSize of
        GT -> dropJ (n-lSize) r
        EQ -> r
        LT -> Append m (dropJ n l) r
    where
        lSize = getSize $ size $ tag l

---------- ex 2.3 ----------
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ 0 jl = Empty
takeJ _ jl@(Single _ _) = jl
takeJ n jl@(Append m l r ) =
    if mSize == n
    then jl
    else
        case compare n lSize of
            GT -> Append m l $ takeJ (n-lSize) r
            EQ -> l
            LT -> takeJ n l
    where
        lSize = getSize $ size $ tag l
        mSize = getSize $ size $ m

---------- ex 3 ----------
-- Testing Score from Scrabble module
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

---------- ex 4 ----------
instance Buffer (JoinList (Score,Size) String) where
    toString Empty          = ""
    toString (Single _ x)   = x
    toString (Append _ x y) = toString x ++ toString y

    fromString = buildBuffer . map (\s -> Single (scoreString s, Size 1) s) . lines
        where
            buildBuffer ::[JoinList (Score,Size) String] -> JoinList (Score,Size) String
            buildBuffer [] = Empty
            buildBuffer [x] = x
            buildBuffer xs = buildBuffer half +++ buildBuffer otherHalf
                where
                    l = length xs
                    half = take (div l 2) xs
                    otherHalf = drop (div l 2) xs

    line = indexJ

    replaceLine _ _ Empty = Empty
    replaceLine i _ _ | i < 0 = Empty
    replaceLine 0 newLine (Single m _) = Single m newLine
    replaceLine _ newLine (Single _ _) = Empty
    replaceLine i newLine jl@(Append m l r) =
        if (i+1) > mainSize
        then jl
        else
            case compare (i+1) lSize of
                GT -> Append m l (replaceLine (i-lSize) newLine r)
                _  -> Append m (replaceLine i newLine l) r
        where
            mainSize = getSize $ size m
            lSize = getSize $ size $ tag l

    numLines Empty              = 0
    numLines (Single (_,s) _)   = getSize s
    numLines (Append (_,s) _ _) = getSize s

    value Empty              = 0
    value (Single (s,_) _)   = getScore s
    value (Append (s,_) _ _) = getScore s

main :: IO ()
main = runEditor editor initialBuffer

initialBuffer :: JoinList (Score,Size) String
initialBuffer = fromString initialString

initialString :: String
initialString =
    unlines
        [ "This buffer is for notes you don't want to save, and for"
        , "evaluation of steam valve coefficients."
        , "To load a different file, type the character L followed"
        , "by the name of the file."
        ]
