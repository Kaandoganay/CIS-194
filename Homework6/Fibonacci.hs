
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
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
    show = show .  streamToList

--exercise4--x

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs) 

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

--exercise5--
nats :: Stream Integer
nats = streamFromSeed (+1) 0


ruler :: Stream Integer
ruler = streamMap f $ streamFromSeed (+1) 1
  where f x | odd x = 0
            | otherwise = 1 + f (x `div` 2)

--exercise6--
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))
