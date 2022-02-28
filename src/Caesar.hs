module Caesar
  ( cipherIO
  ) where
import           Data.Char
import           Data.List
import           Text.Read

alphabet = "abcdefghijklmnopqrstuvwxyz"

shiftSingleChar key op c = do
  let maybeIdx = elemIndex c alphabet
  let abcLen   = length alphabet
  case maybeIdx of
    Just idx -> alphabet !! (((op idx key `rem` abcLen) + abcLen) `rem` abcLen)
    Nothing  -> error "WTF"

cipher text key op = map (shiftSingleChar key op) text

encrypt plaintext key = cipher plaintext key (+)
decrypt ciphertext key = cipher ciphertext key (-)


cipherIO t = case t of
  "1" -> encryptIO
  "2" -> decryptIO
  _   -> error "Dude pls!"

encryptIO = do
  putStrLn "Plaintext:"
  plaintext <- getLine
  putStrLn "Key(number):"
  keyStr <- getLine
  let key = case (readMaybe keyStr :: Maybe Int) of
        Just shift -> shift
        Nothing    -> error "nothing"
  putStrLn (encrypt plaintext key)

decryptIO = do
  putStrLn "Ciphertext:"
  plaintext <- getLine
  putStrLn "Key(number):"
  keyStr <- getLine
  let key = case (readMaybe keyStr :: Maybe Int) of
        Just shift -> shift
        Nothing    -> error "nothing"
  putStrLn (decrypt plaintext key)
