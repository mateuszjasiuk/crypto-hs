module Des.Common
  ( permutate
  ) where

import           Data.Bits
import           Data.Word

-- Permuatation helpers
permToMap :: [Int] -> [(Int, Int)]
permToMap p = zip p [0 .. (length p)]

permutateOne :: Word64 -> Int -> Int -> (Int, Int) -> Word64
permutateOne word from to (x, y) =
  (word .&. bit (from - x)) `shift` (to - (from - x) - y - 1)

permutate :: Word64 -> [Int] -> Int -> Int -> Word64
permutate word p fromBits toBits =
  foldr (xor . permutateOne word fromBits toBits) (0 :: Word64) (permToMap p)
