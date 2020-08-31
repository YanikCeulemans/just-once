module SharedTypes (HexString(..), Bit(..), fromInt) where

import Prelude

newtype HexString = HexString String

instance showHexString :: Show HexString where
  show (HexString s) = show s

instance eqHexString :: Eq HexString where
  eq (HexString a) (HexString b) = eq a b

data Bit = One | Zero


instance showBit :: Show Bit where
  show One = "1"
  show Zero = "0"


instance eqBit :: Eq Bit where
  eq One One = true
  eq Zero Zero = true
  eq _ _ = false


fromInt :: Int -> Bit
fromInt 0 = Zero
fromInt _ = One


toInt :: Bit -> Int
toInt One = 1
toInt Zero = 0