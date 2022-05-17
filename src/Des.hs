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

testM :: Word64
testM = 0x0123456789ABCDEF

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

split64Half :: Word64 -> (Word64, Word64)
split64Half key =
  ((key .&. 0xFFFFFFFF00000000) `shiftR` 32, key .&. 0xFFFFFFFF)

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

des :: Word64 -> Word64 -> Word64
des mR32 key48 = permutate mR32 expDBox 32 48 `xor` key

encrypt :: Word64 -> Word64 -> (Word64, Word64)
encrypt plaintext key = do
  let initial = permutate plaintext initialPerm 64 64
  let keys    = generateKeys key
  split64Half initial

sBoxRow :: Word64 -> Word64
sBoxRow m6bit = m6bit .&. 0x01 .|. (m6bit .&. 0x21) `shiftR` 4

sBoxCol :: Word64 -> Word64
sBoxCol m6bit = (m6bit .&. 0x1E) `shiftR` 1

sBoxVal box m6Bit = do
  let col = sBoxCol m6Bit
  let row = sBoxRow m6Bit
  box !! fromIntegral col !! fromIntegral row

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

expDBox :: [Int]
expDBox =
  [ 32
  , 1
  , 2
  , 3
  , 4
  , 5
  , 4
  , 5
  , 6
  , 7
  , 8
  , 9
  , 8
  , 9
  , 10
  , 11
  , 12
  , 13
  , 12
  , 13
  , 14
  , 15
  , 16
  , 17
  , 16
  , 17
  , 18
  , 19
  , 20
  , 21
  , 20
  , 21
  , 22
  , 23
  , 24
  , 25
  , 24
  , 25
  , 26
  , 27
  , 28
  , 29
  , 28
  , 29
  , 30
  , 31
  , 32
  , 1
  ]

sbox :: [[[Word64]]]
sbox =
  [ [ [14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7]
    , [0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8]
    , [4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0]
    , [15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13]
    ]
  , [ [15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10]
    , [3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5]
    , [0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15]
    , [13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9]
    ]
  , [ [10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8]
    , [13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1]
    , [13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7]
    , [1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12]
    ]
  , [ [7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15]
    , [13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9]
    , [10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4]
    , [3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14]
    ]
  , [ [2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9]
    , [14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6]
    , [4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14]
    , [11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3]
    ]
  , [ [12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11]
    , [10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8]
    , [9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6]
    , [4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13]
    ]
  , [ [4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1]
    , [13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6]
    , [1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2]
    , [6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12]
    ]
  , [ [13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7]
    , [1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2]
    , [7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8]
    , [2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11]
    ]
  ]
