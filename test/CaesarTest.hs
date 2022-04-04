module CaesarTest where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Caesar.Core                   as Caesar

arbStr :: Gen String
arbStr = listOf $ elements alpha where alpha = ['a' .. 'z']
arbTxt :: Gen Caesar.Text
arbTxt = do
  Caesar.Text <$> arbStr

instance Arbitrary Caesar.Text where
  arbitrary = arbTxt

prop_encryptDecrypt :: Caesar.Text -> Int -> Bool
prop_encryptDecrypt text key =
  Caesar.decrypt (Caesar.encrypt text key) key == text

