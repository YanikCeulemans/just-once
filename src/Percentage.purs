module Percentage (Percentage, fromInt, toInt) where

import Prelude

-- | A percentage represented by whole numbers between 0 and 100 inclusive
newtype Percentage = Percentage Int

derive instance eqPercentage :: Eq Percentage

derive instance ordPercentage :: Ord Percentage

instance boundedPercentage :: Bounded Percentage where
  top = Percentage 100
  bottom = Percentage 0

fromInt :: Int -> Percentage
fromInt = Percentage >>> clamp bottom top

toInt :: Percentage -> Int
toInt (Percentage p) = p
