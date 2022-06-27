module Des.Key.InternalTest where

import           Test.Hspec                    as H
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

import           Data.Word

import           Des.Key.Internal              as Key

spec_split56Half :: Spec
spec_split56Half = describe "split56Half" $ do
  it "should split value 0xFF0000000000FF to (0xFF00000, 0x00000FF)" $ do
    let val = 0xFF0000000000FF
    Key.split56Half val `shouldBe` (0xFF00000, 0x00000FF)

spec_parityDrop :: Spec
spec_parityDrop = describe "parityDrop" $ do
  it "should permutate key 0x133457799BBCDFF1 to 0xF0CCAAF556678F" $ do
    let key = 0x133457799BBCDFF1
    Key.parityDrop key `shouldBe` 0xF0CCAAF556678F

