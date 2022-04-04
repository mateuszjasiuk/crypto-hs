module Common
  ( charByIdx
  , abc
  ) where

abc :: String
abc = ['a' .. 'z']

abcLen :: Int
abcLen = length abc

charByIdx :: Int -> Char
charByIdx shift = abc !! (((shift `rem` abcLen) + abcLen) `rem` abcLen)
