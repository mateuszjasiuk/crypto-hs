module Des.Key
  ( generateKeys
  ) where

import           Data.Bits
import           Data.Word

import           Des.Common                     ( permutate )
import           Des.Key.Internal

rotateNthLeft :: Int -> Word64 -> Word64 -> Word64
rotateNthLeft nth mask word = do
  let nth'        = nth - 1
      -- Need to rotate by shifting because the type is Word64 and
      -- word itself might be "smaller"
      rotatedBit  = (bit nth' .&. word) `shiftR` nth'
      -- We are shifting to the left to imitate rotation
      shiftedWord = word `shiftL` 1
  -- Here we are adding rotated bit to the shifted word and
  -- applying mask so we zero leftmost bit
  (rotatedBit .|. shiftedWord) .&. mask

shiftKeyHalf :: Word64 -> Int -> Word64
shiftKeyHalf key shift = shiftKeyHalf' shift key
 where
  shiftKeyHalf' 0 res = res
  -- `shift` is a value taken from shift table, so we either iterate once or twice
  shiftKeyHalf' shift res =
    shiftKeyHalf' (shift - 1) (rotateNthLeft 28 0xFFFFFFF res)

shiftKeyHalves :: (Word64, Word64) -> Int -> (Word64, Word64)
shiftKeyHalves (key1, key2) shift =
  (shiftKeyHalf key1 shift, shiftKeyHalf key2 shift)

joinKeyHalves :: (Word64, Word64) -> Word64
joinKeyHalves (key1, key2) = (key1 `shiftL` 28) .|. key2

compressKey :: Word64 -> Word64
compressKey key = permutate key compBox 56 48

generateKeys' :: Int -> [Word64] -> (Word64, Word64) -> [Word64]
-- There are 16 des rounds so we need 16 round keys
generateKeys' 16 roundKeys _         = roundKeys
generateKeys' n  roundKeys keyHalves = do
  let shifted  = shiftKeyHalves keyHalves $ shiftTable !! n
      -- We need to compress key from 56 to 48 bits to match the message length
      -- in the round function
      roundKey = compressKey . joinKeyHalves $ shifted
  generateKeys' (n + 1) (roundKeys ++ [roundKey]) shifted

generateKeys :: Word64 -> [Word64]
generateKeys key = do
  -- We are dropping every 8th bit from the key to produce 56bit value
  -- the we are splitting it to two 28bit halves
  let initialKey = split56Half . parityDrop $ key
  generateKeys' 0 [] initialKey

compBox =
  [ 14
  , 17
  , 11
  , 24
  , 1
  , 5
  , 3
  , 28
  , 15
  , 6
  , 21
  , 10
  , 23
  , 19
  , 12
  , 4
  , 26
  , 8
  , 16
  , 7
  , 27
  , 20
  , 13
  , 2
  , 41
  , 52
  , 31
  , 37
  , 47
  , 55
  , 30
  , 40
  , 51
  , 45
  , 33
  , 48
  , 44
  , 49
  , 39
  , 56
  , 34
  , 53
  , 46
  , 42
  , 50
  , 36
  , 29
  , 32
  ]

shiftTable = [1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1]
