{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Text.Read

---------- ex 1 ----------

parseError :: String -> LogMessage
parseError string =
        readError $ words string
    where

        helper :: MessageType -> [String] -> LogMessage
        helper err (t:rest) =
            case (readMaybe t :: Maybe Int) of
                Nothing  -> Unknown string
                Just int -> LogMessage err int $ unwords rest
        helper _ _ = Unknown string

        readError :: [String] -> LogMessage
        readError ("I":xs) = helper Info xs
        readError ("W":xs) = helper Warning xs
        readError ("E":n:xs) =
            case (readMaybe n :: Maybe Int) of
                Nothing  -> Unknown string
                Just int -> helper (Error int) xs
        readError _ = Unknown string

parse :: String -> [LogMessage]
parse log = map parseError (lines log)

---------- ex 2 ----------

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert log Leaf = Node Leaf log Leaf
insert log@(LogMessage _ time _) (Node left m@(LogMessage _ treeTime _ ) right)
    | time < treeTime = Node (insert log left) m right
    | time >= treeTime = Node left m (insert log right)

---------- ex 3 ----------

build :: [LogMessage] -> MessageTree
build list = foldr (insert) Leaf list

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


