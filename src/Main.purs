module Main where

import Prelude

import Data.Maybe (Maybe(..), isNothing)
import Effect (Effect)
import Effect.Console (logShow)
import Totp (runTotp)

main :: Effect Unit
main = do
  totp <- runTotp
  case totp of
    Nothing -> pure unit
    Just t -> logShow t
