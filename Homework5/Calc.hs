module Calc where

import ExprT
import Parser
import StackVM
import Data.Maybe
import qualified Data.Map
import qualified Distribution.PackageDescription as Exprt

--Exercise1--
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = eval x  + eval y
eval (ExprT.Mul x y) = eval x * eval y

--Exercise2--

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

--Exercise3--
class Expr a where
    lit :: Integer-> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

--Exercise4--
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (<= 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)


instance Expr MinMax where
   
   add (MinMax x) (MinMax y) = MinMax $ max x y
   mul (MinMax x) (MinMax y) = MinMax $ min x y
   lit                       = MinMax

instance Expr Mod7 where
    add (Mod7 x) (Mod7 y) = Mod7 $ (x+y) `mod` 7
    mul (Mod7 x) (Mod7 y) = Mod7 $ (x*y) `mod` 7
    lit   x               = Mod7 $ x `mod` 7



