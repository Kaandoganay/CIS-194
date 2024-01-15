{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Text.XHtml (treeColors)

--Exercise1--
parseMessage :: String -> LogMessage
parseMessage string = case words string of 
     ("E":st: sy : msg) -> LogMessage (Error (read st))  (read sy) (unwords msg)
     ("I":sy :msg) -> LogMessage Info (read sy) (unwords msg)
     ("W":sy : msg) -> LogMessage Warning (read sy) (unwords msg)
     (_) -> Unknown "This is not in the right format"


parse :: String -> [LogMessage]
parse string = map parseMessage  (lines string)

--Exercise2--
insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert lmsg1@(LogMessage _ ts1 _) (Node left lmsg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left lmsg2 (insert lmsg1 right)
  | otherwise = Node (insert lmsg1 left) lmsg2 right
insert _ tree = tree

--Exercise3--