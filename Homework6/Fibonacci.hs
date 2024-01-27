{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Fibonacci where
--exercise1--
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--exercise2--
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

--exercise3--
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList