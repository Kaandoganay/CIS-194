{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

--Exercise1--
parseMessage :: String -> LogMessage
parseMessage string = case words string of 
     ("E":st: sy : msg) -> LogMessage (Error (read st))  (read sy) (unwords msg)
     ("I":sy :msg) -> LogMessage Info (read sy) (unwords msg)
     ("W":sy : msg) -> LogMessage Warning (read sy) (unwords msg)
     (_) -> Unknown "This is not in the right format"

