module Totp (runTotp) where

import Base32 (decode)
import Data.Array (length, replicate, unsnoc, (!!))
import Data.Int (pow)
import Data.Int.Bits (and, shl, (.&.), (.|.))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as S
import Data.String.CodeUnits (takeRight)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console (logShow)
import Node.Buffer (Buffer, create, toArray)
import Prelude (Unit, bind, flip, mod, otherwise, pure, show, unit, when, ($), (+), (-), (<$>), (<*), (<<<), (<>), (==), (>>=))
import SharedTypes (HexString(..))

runTotp :: Effect (Maybe String)
runTotp = do
  case decode "MFKWMTLRJ5KUWNCYNV4EKT2KNZFS6TSI" of
    Nothing -> pure Nothing
    Just secret -> do
      hmac <- createHmac secret >>= toArray
      pure $ createHotp hmac



createHotp :: Array Int -> Maybe String
createHotp hmacArr
  | length hmacArr == 20 = do
    { init, last } <- unsnoc hmacArr
    let offset = last .&. 0xf
    binCode <- getBinCode offset init
    let hotp = show $ binCode `mod` (10 `pow` 6)
    pure $ takeRight 6 $ (S.joinWith "" $ replicate 6 "0") <> hotp
  | otherwise = Nothing


getBinCode :: Int -> Array Int -> Maybe Int
getBinCode offset initHmacArr = do
    b1 <- flip shl 24 <<< and 0x7f <$> initHmacArr !! offset
    b2 <- flip shl 16 <<< and 0xff <$> initHmacArr !! (offset + 1)
    b3 <- flip shl 8 <<< and 0xff <$> initHmacArr !! (offset + 2)
    b4 <- and 0xff <$> initHmacArr !! (offset + 3)
    pure $ b1 .|. b2 .|. b3 .|. b4


foreign import createHmac :: HexString -> Effect Buffer