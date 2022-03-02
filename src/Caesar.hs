module Caesar
  ( cipherIO
  ) where
import           Data.Char
import           Data.List
import           Text.Read

alphabet = "abcdefghijklmnopqrstuvwxyz"
abcLen = length alphabet

type Op = Int -> Int -> Int
type Key = Int
type Text = String
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

-- TODO: https://stackoverflow.com/questions/14379185/function-privacy-and-unit-testing-haskell
cipher :: Text -> Key -> Op -> Text
cipher text key op = map (shiftSingleChar key op) text

encrypt :: Plaintext -> Key -> Ciphertext
encrypt plaintext key = cipher plaintext key (+)

decrypt :: Ciphertext -> Key -> Plaintext
decrypt ciphertext key = cipher ciphertext key (-)

-- TODO: move strings to consts somewhere
cipherIO :: String -> IO ()
cipherIO t = case t of
  "1" -> encryptIO
  "2" -> decryptIO
  _   -> error "Dude pls!"

encryptIO :: IO ()
encryptIO = do
  putStrLn "Plaintext:"
  plaintext <- getLine
  putStrLn "Key(number):"
  keyStr <- getLine
  let key = case (readMaybe keyStr :: Maybe Int) of
        Just shift -> shift
        Nothing    -> error "nothing"
  putStrLn (encrypt plaintext key)

decryptIO :: IO ()
decryptIO = do
  putStrLn "Ciphertext:"
  plaintext <- getLine
  putStrLn "Key(number):"
  keyStr <- getLine
  let key = case (readMaybe keyStr :: Maybe Int) of
        Just shift -> shift
        Nothing    -> error "nothing"
  putStrLn (decrypt plaintext key)
