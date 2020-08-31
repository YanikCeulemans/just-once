module Test.Main where

import Prelude

import Base32 (AccumulateDirection(..), decode, encode, groupsOf, octetToBits)
import Data.Maybe (Maybe(..))
import Data.Result (Result(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import SharedTypes (Bit(..), HexString(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldContain)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [consoleReporter] do
    describe "base32" do
      it "should correctly decode a base32 encoded string" do
        decode "MFKWMTLRJ5KUWNCYNV4EKT2KNZFS6TSI" `shouldEqual` Just (HexString "6155664d714f554b34586d78454f4a6e4b2f4e48")

      it "should correctly encode to a base32 encoded string" do
        (encode $ HexString "6155664d714f554b34586d78454f4a6e4b2f4e48") `shouldEqual` (Just "MFKWMTLRJ5KUWNCYNV4EKT2KNZFS6TSI")
        (encode $ HexString "6e6f6465") `shouldEqual` (Just "NZXWIZI=")

      it "should correctly do groupsOf" do
        groupsOf 2 AccumulateRight [1,2,3,4] `shouldEqual` [[1,2], [3,4]]
        groupsOf 3 AccumulateRight [1,2,3,4] `shouldEqual` [[1],[2,3,4]]
        groupsOf 0 AccumulateRight [1,2,3,4] `shouldEqual` []
        groupsOf (-1) AccumulateRight [1,2,3,4] `shouldEqual` []
        groupsOf 3 AccumulateLeft [1,2,3,4] `shouldEqual` [[1,2,3], [4]]

      it "should correctly covert octet to bits" do
        (octetToBits 0x80) `shouldEqual` [One, Zero, Zero, Zero, Zero, Zero, Zero, Zero]
        (octetToBits 0x03) `shouldEqual` [Zero, Zero, Zero, Zero, Zero, Zero, One, One]
