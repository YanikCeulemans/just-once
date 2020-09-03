module Totp (runTotp, totpConfig, TotpConfig) where

import Base32 (decode)
import Data.Array (length, replicate, unsnoc, (!!))
import Data.DateTime.Instant (Instant, unInstant)
import Data.Int (floor, hexadecimal, pow, toStringAs)
import Data.Int.Bits (and, shl, (.&.), (.|.))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String as S
import Data.String.CodeUnits (takeRight)
import Data.Time.Duration (Milliseconds, Seconds(..), convertDuration, toDuration)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Now (now)
import Node.Buffer (Buffer, toArray)
import Prelude (bind, flip, mod, otherwise, pure, show, (#), ($), (+), (-), (/), (<#>), (<$>), (<<<), (<>), (==), (>), (>>=))
import SharedTypes (HexString(..))


newtype TotpConfig = TotpConfig
  { t0 :: Number
  , step :: Number
  , secret :: String
  }


totpConfig :: String -> TotpConfig
totpConfig secret = TotpConfig
  { t0 : 0.0
  , step : 30.0
  , secret
  }


runTotp :: TotpConfig -> Effect (Maybe String)
runTotp (TotpConfig { t0, step, secret }) = do
  counter <- now <#> counterHexString t0 step
  case decode secret of
    Nothing -> pure Nothing
    Just decodedSecret -> do
      hmac <- createHmac { secret: decodedSecret, counter } >>= toArray
      pure $ createHotp hmac


counterHexString :: Number -> Number -> Instant -> HexString
counterHexString t0 step present =
  let
    duration = unInstant present # toDuration :: Milliseconds

    (Seconds seconds) = convertDuration duration
    t = floor $ (seconds - t0) / step
  in
    toStringAs hexadecimal t # padStartWith 16 "0" # HexString


padStartWith :: Int -> String -> String -> String
padStartWith count padChar input
  | count > S.length input = (replicate (count - S.length input) padChar # joinWith "") <> input
  | otherwise = input


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


foreign import createHmac :: { secret :: HexString, counter :: HexString } -> Effect Buffer