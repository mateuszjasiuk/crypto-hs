module Vigenere.Core
  ( encrypt
  , decrypt
  ) where
import qualified Common
import           Data.List

type Op = Int -> Int -> Int
type Key = String
type Text = String
type Ciphertext = Text
type Plaintext = Text

charPosition :: Char -> Int
charPosition c = do
  case maybeIdx of
    Just idx -> idx
    Nothing  -> error "WTF"
  where maybeIdx = elemIndex c Common.abc

shiftChar :: Op -> Char -> Int -> Char
shiftChar op char shift = do
  Common.charByIdx idx
  where idx = op (charPosition char) shift

cipher :: Text -> Key -> Op -> Text
cipher text key op = do
  zipWith (shiftChar op) text shiftCycles
  where shiftCycles = take (length text) $ cycle $ map charPosition key

encrypt :: Plaintext -> Key -> Ciphertext
encrypt plaintext key = cipher plaintext key (+)

decrypt :: Ciphertext -> Key -> Plaintext
decrypt plaintext key = cipher plaintext key (-)

