module Totp (runTotp, totpConfig, toSecret, TotpConfig, Secret) where

import Base32 (decode)
import Data.Array (length, replicate, unsnoc, (!!))
import Data.DateTime (Second, time)
import Data.DateTime.Instant (Instant, toDateTime, unInstant)
import Data.Enum (fromEnum, toEnum)
import Data.Int (floor, hexadecimal, pow, round, toStringAs)
import Data.Int.Bits (and, shl, (.&.), (.|.))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String as S
import Data.String.CodeUnits (takeRight)
import Data.Time (second)
import Data.Time.Duration (Milliseconds, Seconds(..), convertDuration, toDuration)
import Effect (Effect)
import Effect.Now (now)
import Node.Buffer (Buffer, toArray)
import Prelude (bind, flip, mod, otherwise, pure, show, (#), ($), (+), (-), (/), (<#>), (<$>), (<<<), (<>), (==), (>), (>>=))
import SharedTypes (HexString(..))


newtype Secret = Secret String


toSecret :: String -> Maybe Secret
toSecret encoded = decode encoded <#> (\(HexString s) -> Secret s)


newtype TotpConfig = TotpConfig
  { t0 :: Number
  , step :: Number
  , secret :: Secret
  }


totpConfig :: Secret -> TotpConfig
totpConfig s = TotpConfig
  { t0 : 0.0
  , step : 30.0
  , secret : s
  }


type Totp =
  { code :: String
  , validFor :: Second
  }


runTotp :: TotpConfig -> Effect (Maybe Totp)
runTotp (TotpConfig { t0, step, secret }) = do
  { counter, currentSeconds } <- now <#> totpTimeData
  hmac <- createHmac secret counter >>= toArray
  pure $ do
    hotp <- createHotp hmac
    validFor <- calcValidFor (round step) currentSeconds
    pure { code: hotp, validFor }
  where
    calcValidFor :: Int -> Second -> Maybe Second
    calcValidFor iStep seconds
      | iStep > 0 =
          iStep - (fromEnum seconds `mod` iStep) # toEnum
      | otherwise  = Nothing

    totpTimeData :: Instant -> { counter :: HexString, currentSeconds :: Second }
    totpTimeData instant =
      { counter, currentSeconds }
      where
        counter = counterHexString t0 step instant
        currentSeconds = toDateTime instant # time # second



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


foreign import createHmac :: Secret -> HexString -> Effect Buffer