{-# OPTIONS_GHC -Wall #-}
module Week2.LogAnalysis where

import Text.Read
import Week2.Log

---------- ex 1 ----------

parseError :: String -> LogMessage
parseError string =
    case words string of
        "I":n:xs -> maybe (Unknown string) (makeLog Info xs) (readMaybe n :: Maybe Int)
        "W":n:xs -> maybe (Unknown string) (makeLog Warning xs) (readMaybe n :: Maybe Int)
        "E":m:n:xs -> maybe (Unknown string) helper (readMaybe m::Maybe Int)
            where
                helper nn = maybe (Unknown string) (makeLog (Error nn) xs) (readMaybe n:: Maybe Int)
        _ -> Unknown string
    where
        makeLog x xs t = LogMessage x t $ unwords xs

parse :: String -> [LogMessage]
parse = map parseError . lines

---------- ex 2 ----------

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert log Leaf = Node Leaf log Leaf
insert log@(LogMessage _ time _) (Node left m@(LogMessage _ treeTime _ ) right) =
    case compare time treeTime of
        LT -> Node (insert log left) m right
        _  -> Node left m (insert log right)

---------- ex 3 ----------

build :: [LogMessage] -> MessageTree
build list = foldr insert Leaf list

---------- ex 4 ----------

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf                      = []
inOrder (Node left message right) = (inOrder left)++[message]++(inOrder right)

---------- ex 5 ----------

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages =
        map getText sortedMessages
    where
        sortedMessages =  (dropWhile isBelow50) $ inOrder $ build messages
        isBelow50 (LogMessage _ number _) = number < 50
        getText (LogMessage _ _ text) = text
