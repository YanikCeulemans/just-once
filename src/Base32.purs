module Base32 (encode, decode, octetToBits, groupsOf, AccumulateDirection(..), bitGroupsToOctets) where

import Prelude
import Data.Array (drop, elemIndex, foldl, foldr, length, reverse, take, uncons, (!!), (:))
import Data.Array.NonEmpty (replicate, toArray)
import Data.ByteString (Encoding(..), fromString, pack, toString, unpack)
import Data.Either (hush)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Int (fromNumber, toNumber)
import Data.Int.Bits (and, or, shl, shr)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (sequence, traverse)
import Math as Math
import SharedTypes (Bit(..), HexString(..), fromInt)
import Type.Quotient (mkQuotient, runQuotient)

base32Alphabet :: Array String
base32Alphabet = split (Pattern "") "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

base32Pad :: String
base32Pad = "="

encode :: HexString -> Maybe String
encode (HexString s) = do
  originalOctets <- map runQuotient <<< unpack <$> fromString s Hex
  resultLength <- length originalOctets # toNumber >>> flip div 5.0 >>> Math.ceil >>> fromNumber <#> (*) 8
  let
    bitGroups = groupsOf 5 AccumulateLeft $ originalOctets >>= octetToBits

    octets = bitGroupsToOctets $ padRight 5 <$> bitGroups
  chars <- traverse inAlphabet octets
  pure <<< joinWith "" $ take resultLength $ chars <> toArray (replicate 8 base32Pad)
  where
  inAlphabet index = base32Alphabet !! index

padRight :: Int -> Array Bit -> Array Bit
padRight count bits = take 5 (bits <> toArray (replicate count Zero))

decode :: String -> Maybe HexString
decode input = do
  decodeRegex <- hush $ regex "(.+?)(=*)$" noFlags
  { chars, takeCount } <- match decodeRegex input <#> toArray >>= sequence >>= matchDecodeInput
  let
    charArray = split (Pattern "") chars
  indices <- traverse (flip elemIndex base32Alphabet) charArray
  let
    indexBits = indices >>= (octetToBits >>> drop 3) # groupsOf 8 AccumulateLeft

    octets = bitGroupsToOctets indexBits

    bytestring = flip toString Hex <<< pack $ mkQuotient <$> (take (length octets - takeCount) octets)
  pure $ HexString bytestring
  where
  matchDecodeInput :: Array String -> Maybe { chars :: String, takeCount :: Int }
  matchDecodeInput [ _, chars, "" ] = Just { chars, takeCount: 0 }

  matchDecodeInput [ _, chars, _ ] = Just { chars, takeCount: 1 }

  matchDecodeInput _ = Nothing

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

    finishGrp { result, grp } = (applyDirection direction grp) : result
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
