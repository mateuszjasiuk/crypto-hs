module Caesar.Core
  ( Text(..)
  , encrypt
  , decrypt
  ) where
import           Data.Char
import           Data.List
import           Text.Read

alphabet = ['a' .. 'z']
abcLen = length alphabet

type Op = Int -> Int -> Int
type Key = Int
-- TODO: Not sure if it makes sense to wrap String, will leave for now
newtype Text = Text String
  deriving (Eq, Ord, Show)

mapText :: (Char -> Char) -> Text -> Text
mapText f (Text x) = Text (map f x)

type Ciphertext = Text
type Plaintext = Text

shiftSingleChar :: Key -> Op -> Char -> Char
shiftSingleChar key op c = do
  case maybeIdx of
    -- TODO: move this to common module
    -- TODO: try natural numbers?
    Just idx -> alphabet !! (((op idx key `rem` abcLen) + abcLen) `rem` abcLen)
    Nothing  -> error "WTF"
  where maybeIdx = elemIndex c alphabet

cipher :: Text -> Key -> Op -> Text
cipher text key op = mapText (shiftSingleChar key op) text

encrypt :: Plaintext -> Key -> Ciphertext
encrypt plaintext key = cipher plaintext key (+)

decrypt :: Ciphertext -> Key -> Plaintext
decrypt ciphertext key = cipher ciphertext key (-)

-- TODO: https://stackoverflow.com/questions/14379185/function-privacy-and-unit-testing-haskell
