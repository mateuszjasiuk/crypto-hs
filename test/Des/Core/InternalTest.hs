module Des.Core.InternalTest where

import           Test.Hspec                    as H
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

import           Data.Word

import           Des.Core.Internal             as Des

spec_split64Half :: Spec
spec_split64Half = describe "split64Half" $ do
  it "should split value 0xFF000000000000FF to (0xFF000000, 0x000000FF)" $ do
    let val = 0xFF000000000000FF
    Des.split64Half val `shouldBe` (0xFF000000, 0x000000FF)

