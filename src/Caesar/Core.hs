module Caesar.Core
  ( Text(..)
  , encrypt
  , decrypt
  ) where
import qualified Common
import           Data.Char
import           Data.List
import           Text.Read

type Op = Int -> Int -> Int
type Key = Int
-- TODO: Not sure if it makes sense to wrap String, will leave for now
-- TODO: Move to common?
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
    Just idx -> Common.charByIdx $ op idx key
    Nothing  -> error "WTF"
  where maybeIdx = elemIndex c Common.abc

cipher :: Text -> Key -> Op -> Text
cipher text key op = mapText (shiftSingleChar key op) text

encrypt :: Plaintext -> Key -> Ciphertext
encrypt plaintext key = cipher plaintext key (+)

decrypt :: Ciphertext -> Key -> Plaintext
decrypt ciphertext key = cipher ciphertext key (-)

-- TODO: https://stackoverflow.com/questions/14379185/function-privacy-and-unit-testing-haskell
