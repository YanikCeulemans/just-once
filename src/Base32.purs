module Base32 (encode, decode, octetToBits, groupsOf, AccumulateDirection(..), bitGroupsToOctets) where

import Prelude

import Data.Array (concat, drop, elemIndex, foldl, foldr, length, null, reverse, uncons, zip, (!!), (:))
import Data.ByteString (Encoding(..), fromString, pack, toString, unpack)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int.Bits (and, or, shl, shr)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (traverse)
import Debug.Trace (spy)
import SharedTypes (Bit(..), HexString(..), fromInt)
import Type.Quotient (mkQuotient, runQuotient)

base32Alphabet :: Array String
base32Alphabet = split (Pattern "") "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

base32Pad :: Char
base32Pad = '='

-- TODO: Padding
encode :: HexString -> Maybe String
encode (HexString s) = do
  originalOctets <- map runQuotient <<< unpack <$> fromString s Hex
  let
    bitGroups = groupsOf 5 AccumulateRight $ originalOctets >>= octetToBits

    octets = bitGroupsToOctets $ spy "bitGroups" bitGroups
  chars <- traverse inAlphabet octets
  pure $ joinWith "" chars
  where
  inAlphabet index = base32Alphabet !! index

decode :: String -> Maybe HexString
decode input = do
  let
    chars = split (Pattern "") input
  indices <- traverse (flip elemIndex base32Alphabet) chars
  let
    indexBits = groupsOf 8 AccumulateRight $ concat (drop 3 <<< octetToBits <$> indices)

    octets = bitGroupsToOctets indexBits

    bytestring = flip toString Hex <<< pack $ mkQuotient <$> octets
  pure $ HexString bytestring

octetToBits :: Int -> Array Bit
octetToBits = octetToBits' 7 []
  where
  octetToBits' :: Int -> Array Bit -> Int -> Array Bit
  octetToBits' -1 result _ = result

  octetToBits' shiftAmount result octet = octetToBits' (shiftAmount - 1) (bit : result) octet
    where
    bit = fromInt (((0x80 `shr` shiftAmount) `and` octet) `shr` (7 - shiftAmount))

data AccumulateDirection
  = AccumulateLeft
  | AccumulateRight


applyDirection :: forall a. AccumulateDirection -> Array a -> Array a
applyDirection AccumulateLeft xs = reverse xs

applyDirection AccumulateRight xs = xs

groupsOf :: forall a. Int -> AccumulateDirection -> Array a -> Array (Array a)
groupsOf count direction array
  | count > 0 = applyDirection direction <<< finishGrp $ accumulate direction grpFn { result: [], grp: [], index: 0 } array
    where
    accumulate AccumulateLeft f r xs = foldl f r xs
    accumulate AccumulateRight f r xs = foldr (flip f) r xs

    grpFn { result, grp, index } item =
      if index `mod` count == count - 1 then
        { result: applyDirection direction (item : grp) : result, grp: [], index: index + 1 }
      else
        { result, grp: (item : grp), index: index + 1 }

    finishGrp :: { result :: Array (Array a), grp :: Array a, index :: Int } -> Array (Array a)
    finishGrp { result, grp: [] } = result

    finishGrp { result, grp } = grp : result
  | otherwise = []

bitGroupsToOctets :: Array (Array Bit) -> Array Int
bitGroupsToOctets bitGroups = bitGroupsToOctets' [] $ uncons bitGroups
  where
  foldBits :: Int -> Int -> Bit -> Int -> Int
  foldBits _ index Zero acc = acc

  foldBits shiftOffset index One acc = or acc $ 1 `shl` (shiftOffset - index)

  bitGroupsToOctets' :: Array Int -> Maybe { head :: Array Bit, tail :: Array (Array Bit) } -> Array Int
  bitGroupsToOctets' result Nothing = reverse result

  bitGroupsToOctets' result (Just { head, tail }) = bitGroupsToOctets' (foldrWithIndex (foldBits $ length head - 1) 0 head : result) $ uncons tail
