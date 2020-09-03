module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Totp (runTotp, totpConfig)

main :: Effect Unit
main = do
  totp <- runTotp $ totpConfig "MFKWMTLRJ5KUWNCYNV4EKT2KNZFS6TSI"
  case totp of
    Nothing -> pure unit
    Just t -> logShow t
