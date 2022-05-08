module Des
  ( encrypt
  , permutate
  , parityDrop
  , splitKey
  ) where

import           Data.Bits
import           Data.Char                      ( intToDigit )
import           Data.Word
import           Numeric                        ( showHex
                                                , showIntAtBase
                                                )

mask :: Word64
mask = 0x7F

key :: Word64
key = 0x133457799BBCDFF1

toHex int = showHex int ""
toBin int = showIntAtBase 2 intToDigit int ""

permToMap :: [Int] -> [(Int, Int)]
permToMap p = zip p [0 .. (length p)]

permutateOne :: Word64 -> Int -> Int -> (Int, Int) -> Word64
permutateOne word from to (x, y) =
  (word .&. bit (from - x)) `shift` (to - (from - x) - y - 1)

permutate :: Word64 -> [Int] -> Int -> Int -> Word64
permutate word p from to =
  foldr (xor . permutateOne word from to) (0 :: Word64) (permToMap p)

parityDrop :: Word64 -> Word64
parityDrop key = permutate key keyp 64 56

splitKey :: Word64 -> (Word64, Word64)
splitKey key = ((key .&. 0xFFFFFFF0000000) `shiftR` 28, key .&. 0xFFFFFFF)

rotateNthLeft :: Int -> Word64 -> Word64 -> Word64
rotateNthLeft nth mask word = do
  let rotatedBit  = (bit nth .&. word) `shiftR` nth
  let shiftedWord = word `shiftL` 1
  (rotatedBit .|. shiftedWord) .&. mask

shiftKeyHalf :: Word64 -> Int -> Word64
shiftKeyHalf key shift = shiftKeyHalf' shift key
 where
  shiftKeyHalf' 0 res = res
  shiftKeyHalf' shift res =
    shiftKeyHalf' (shift - 1) (rotateNthLeft 27 0xFFFFFFF res)

shiftKeyHalves :: (Word64, Word64) -> Int -> (Word64, Word64)
shiftKeyHalves (key1, key2) shift =
  (shiftKeyHalf key1 shift, shiftKeyHalf key2 shift)

joinKeyHalves :: (Word64, Word64) -> Word64
joinKeyHalves (key1, key2) = (key1 `shiftL` 28) .|. key2

compressKey :: Word64 -> Word64
compressKey key = permutate key compBox 56 48

generateKeys' :: Int -> [Word64] -> (Word64, Word64) -> [Word64]
generateKeys' 16 res _         = res
generateKeys' n  res keyHalves = do
  let shifted  = shiftKeyHalves keyHalves $ shiftTable !! n
      roundKey = compressKey . joinKeyHalves $ shifted
  generateKeys' (n + 1) (res ++ [roundKey]) shifted

generateKeys :: Word64 -> [Word64]
generateKeys key = generateKeys' 0 [] (splitKey . parityDrop $ key)

encrypt :: Word64 -> Word64
encrypt plaintext = permutate plaintext initialPerm 64 64

decrypt :: Word64 -> Word64
decrypt ciphertext = permutate ciphertext finalPerm 64 64

initialPerm =
  [ 58
  , 50
  , 42
  , 34
  , 26
  , 18
  , 10
  , 2
  , 60
  , 52
  , 44
  , 36
  , 28
  , 20
  , 12
  , 4
  , 62
  , 54
  , 46
  , 38
  , 30
  , 22
  , 14
  , 6
  , 64
  , 56
  , 48
  , 40
  , 32
  , 24
  , 16
  , 8
  , 57
  , 49
  , 41
  , 33
  , 25
  , 17
  , 9
  , 1
  , 59
  , 51
  , 43
  , 35
  , 27
  , 19
  , 11
  , 3
  , 61
  , 53
  , 45
  , 37
  , 29
  , 21
  , 13
  , 5
  , 63
  , 55
  , 47
  , 39
  , 31
  , 23
  , 15
  , 7
  ]

finalPerm =
  [ 40
  , 8
  , 48
  , 16
  , 56
  , 24
  , 64
  , 32
  , 39
  , 7
  , 47
  , 15
  , 55
  , 23
  , 63
  , 31
  , 38
  , 6
  , 46
  , 14
  , 54
  , 22
  , 62
  , 30
  , 37
  , 5
  , 45
  , 13
  , 53
  , 21
  , 61
  , 29
  , 36
  , 4
  , 44
  , 12
  , 52
  , 20
  , 60
  , 28
  , 35
  , 3
  , 43
  , 11
  , 51
  , 19
  , 59
  , 27
  , 34
  , 2
  , 42
  , 10
  , 50
  , 18
  , 58
  , 26
  , 33
  , 1
  , 41
  , 9
  , 49
  , 17
  , 57
  , 25
  ]
keyp =
  [ 57
  , 49
  , 41
  , 33
  , 25
  , 17
  , 9
  , 1
  , 58
  , 50
  , 42
  , 34
  , 26
  , 18
  , 10
  , 2
  , 59
  , 51
  , 43
  , 35
  , 27
  , 19
  , 11
  , 3
  , 60
  , 52
  , 44
  , 36
  , 63
  , 55
  , 47
  , 39
  , 31
  , 23
  , 15
  , 7
  , 62
  , 54
  , 46
  , 38
  , 30
  , 22
  , 14
  , 6
  , 61
  , 53
  , 45
  , 37
  , 29
  , 21
  , 13
  , 5
  , 28
  , 20
  , 12
  , 4
  ]

shiftTable = [1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1]

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
