{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module JoinList where

import Sized
import Text.PrettyPrint (sizedText)
--exercise1--
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
xs1 +++ xs2 = Append (tag xs1 <> tag xs2) xs1 xs2

--exercise2--

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                   = Nothing
indexJ k _  | k < 0              = Nothing
indexJ k xs | k >= (getSize . size . tag xs) = Nothing
indexJ _ (Single _ a)            = Just a
indexJ k (Append _ xs1 xs2)
  | k < get                     = indexJ k xs1
  | otherwise                    = indexJ (k - get) xs2
  where get = getSize . size . tag xs1

