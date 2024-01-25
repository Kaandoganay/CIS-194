module Calc where

import ExprT
import Parser
import StackVM
import Data.Maybe
import qualified Data.Map

--Exercise1--
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x  + eval y
eval (ExprT.Mul x y) = eval x * eval y

--Exercise2--

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul