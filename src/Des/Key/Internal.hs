module Des.Key.Internal
  ( split56Half
  ) where

import           Data.Bits
import           Data.Word

split56Half :: Word64 -> (Word64, Word64)
split56Half key = ((key .&. 0xFFFFFFF0000000) `shiftR` 28, key .&. 0xFFFFFFF)
