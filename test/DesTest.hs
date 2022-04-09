module DesTest where

import           Test.Hspec                    as H
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

import           Data.Word

import qualified Des

spec_drop8thBits :: Spec
spec_drop8thBits = describe "drop8thBits" $ do
  it "should reduce key 0x800000000000000F to 0xF" $ do
    let key = 0x800000000000000F
    Des.drop8thBits key `shouldBe` 0xF

  it "should reduce key 0x8000800080008000 to 0" $ do
    let key = 0x8000800080008000
    Des.drop8thBits key `shouldBe` 0


