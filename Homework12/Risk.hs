{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List ( sortBy )

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

maxAttackers :: Army -> Int
maxAttackers n = min 3 (n-1)

maxDefenders :: Army -> Int
maxDefenders = min 2

rollDice :: Int -> Rand StdGen [DieValue]
rollDice n = replicateM n die

sortRolls :: [DieValue] -> [DieValue]
sortRolls = sortBy (flip compare)

matchRolls :: [DieValue] -> [DieValue] -> [Ordering]
matchRolls = zipWith compare

