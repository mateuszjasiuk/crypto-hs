{-# LANGUAGE FlexibleInstances #-}
module VigenereTest where

import           Test.Hspec                    as H
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

import           Vigenere.Core                 as Vigenere

-- newtype Name = Name String

-- instance Arbitrary Name where
--   arbitrary = oneof ["a", "b", "c", "d", "e"]

-- https://stackoverflow.com/questions/20934506/haskell-quickcheck-how-to-generate-only-printable-strings
genSafeChar :: Gen Char
genSafeChar = elements ['a' .. 'z']

genSafeString :: Gen String
genSafeString = listOf genSafeChar

newtype SafeString = SafeString { unwrapSafeString :: String }
    deriving Show

instance Arbitrary SafeString where
  arbitrary = SafeString <$> genSafeString

prop_VigenereEncryptDecryptGeneratedTests :: SafeString -> SafeString -> Bool
prop_VigenereEncryptDecryptGeneratedTests text key = do
  Vigenere.decrypt (Vigenere.encrypt text' key') key' == text'
 where
  text' = unwrapSafeString text
  key'  = unwrapSafeString key

spec_encrypt :: Spec
spec_encrypt = describe "Vigenere.encrypt" $ do
  it "Plaintext: 'irma' Key: 'abc' should return Ciphertext: 'lupd'" $ do
    Vigenere.encrypt "irma" "abc" `shouldBe` "isoa"

  it "Plaintext: 'zzzz' Key: 'b' should return Ciphertext: 'aaaa'" $ do
    Vigenere.encrypt "zzzz" "b" `shouldBe` "aaaa"

  it "Plaintext: 'irma' Key: '' should return Ciphertext: 'irma'" $ do
    Vigenere.encrypt "irma" "" `shouldBe` "irma"

spec_decrypt :: Spec
spec_decrypt = describe "Vigenere.decrypt" $ do
  it "Ciphertext: 'isoa' Key: 'abc' should return Plaintext: 'irma'" $ do
    Vigenere.decrypt "isoa" "abc" `shouldBe` "irma"

  it "Ciphertext: 'aaaa' Key: 'b' should return Plaintext: 'zzzz'" $ do
    Vigenere.decrypt "aaaa" "b" `shouldBe` "zzzz"

  it "Ciphertext: 'irma' Key: '' should return Plaintext: 'irma'" $ do
    Vigenere.decrypt "irma" "" `shouldBe` "irma"

