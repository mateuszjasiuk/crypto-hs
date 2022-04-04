module CaesarTest where

import           Test.Hspec                    as H
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

import           Caesar.Core                   as Caesar

arbStr :: Gen String
arbStr = listOf $ elements alpha where alpha = ['a' .. 'z']

arbTxt :: Gen Caesar.Text
arbTxt = do
  Caesar.Text <$> arbStr

instance Arbitrary Caesar.Text where
  arbitrary = arbTxt

prop_encryptDecryptGeneratedTests :: Caesar.Text -> Int -> Bool
prop_encryptDecryptGeneratedTests text key =
  Caesar.decrypt (Caesar.encrypt text key) key == text

spec_encrypt :: Spec
spec_encrypt = describe "Caesar.encrypt" $ do
  it "Plaintext: 'irma' Key: 3 should return Ciphertext: 'lupd'" $ do
    Caesar.encrypt (Caesar.Text "irma") 3 `shouldBe` Caesar.Text "lupd"

  it "Plaintext: 'zzzz' Key: 1 should return Ciphertext: 'lupd'" $ do
    Caesar.encrypt (Caesar.Text "zzzz") 1 `shouldBe` Caesar.Text "aaaa"

spec_decrypt :: Spec
spec_decrypt = describe "Caesar.decrypt" $ do
  it "Ciphertext: 'lupd' Key: 3 should return Plaintext: 'irma'" $ do
    Caesar.decrypt (Caesar.Text "lupd") 3 `shouldBe` Caesar.Text "irma"

  it "Ciphertext: 'aaaa' Key: 1 should return Plaintext: 'zzzz'" $ do
    Caesar.decrypt (Caesar.Text "aaaa") 1 `shouldBe` Caesar.Text "zzzz"

