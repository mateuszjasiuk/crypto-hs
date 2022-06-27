module Des.Core.Internal
  ( split64Half
  ) where

import           Data.Bits
import           Data.Word

split64Half :: Word64 -> (Word64, Word64)
split64Half key =
  ((key .&. 0xFFFFFFFF00000000) `shiftR` 32, key .&. 0xFFFFFFFF)
