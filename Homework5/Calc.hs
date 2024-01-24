module Calc where

import ExprT
import Parser
import StackVM


--Exercise1--
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x  + eval y
eval (ExprT.Mul x y) = eval x * eval y