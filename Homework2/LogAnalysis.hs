{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

--Exercise1--
parseMessage :: String -> LogMessage
parseMessage string = case words string of
     ("E":st: sy : msg) -> LogMessage (Error (read st))  (read sy) (unwords msg)
     ("I":sy :msg) -> LogMessage Info (read sy) (unwords msg)
     ("W":sy : msg) -> LogMessage Warning (read sy) (unwords msg)
     _ -> Unknown "This is not in the right format"


parse :: String -> [LogMessage]
parse string = map parseMessage  (lines string)

--Exercise2--
insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage {}) Leaf = Node Leaf msg Leaf
insert lmsg1@(LogMessage _ ts1 _) (Node left lmsg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left lmsg2 (insert lmsg1 right)
  | otherwise = Node (insert lmsg1 left) lmsg2 right
insert _ tree = tree

--Exercise3--
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

--Exercise4--
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right ) = inOrder left ++ [msg] ++ inOrder right

--Exercise5--
message :: LogMessage -> String
message (LogMessage _ _ msg) = msg
message (Unknown x) = x

severe :: Int -> LogMessage -> Bool
severe n (LogMessage (Error s) _ _) = n <= s
severe _ _ = False


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . inOrder . build. filter (severe 50)

test :: IO [String]
test = testWhatWentWrong parse whatWentWrong "sample.log"
