module Des
  ( encrypt
  , permutate
  , drop8thBits
  , parityDrop
  , splitKey
  ) where

import           Data.Bits
import           Data.Word

-- 0b01111111
mask :: Word64
mask = 0x7F

key :: Word64
key = 0xFFFFFFFFFFFFFFFF

drop8thBit :: Word64 -> Word64 -> Int -> Word64
drop8thBit key a c = a .|. (key .&. mask `shiftL` (c * 8)) `shiftR` 1

drop8thBits :: Word64 -> Word64
drop8thBits key =
  key .&. mask .|. foldl (drop8thBit key) (0 :: Word64) [1 .. 7]

permToMap :: [Int] -> [(Int, Int)]
permToMap p = zip p [0 .. (length p)]

permutateOne :: Word64 -> (Int, Int) -> Word64
-- (x - 1) and (y - x + 1) because haskell indexes from 0 and perm tables index from 1
permutateOne word (x, y) = (word .&. bit (x - 1)) `shift` (y - x + 1)

permutate :: Word64 -> [Int] -> Word64
permutate word p = foldr (xor . permutateOne word) (0 :: Word64) (permToMap p)

parityDrop :: Word64 -> Word64
parityDrop key = permutate key keyp

-- TODO: Change res to (Word32, Word32)
splitKey :: Word64 -> (Word64, Word64)
splitKey key = (key .&. 0xFFFFFFF, (key .&. 0xFFFFFFF0000000) `shiftR` 28)

encrypt :: Word64 -> Word64
encrypt plaintext = permutate plaintext initialPerm

decrypt :: Word64 -> Word64
decrypt ciphertext = permutate ciphertext finalPerm

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
