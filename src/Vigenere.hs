module Vigenere
  ( cipherIO
  ) where
import           Data.Char
import           Data.List
import           Text.Read

alphabet = "abcdefghijklmnopqrstuvwxyz"

keyToNumbers = map
  (\c -> do
    let maybeIdx = elemIndex c alphabet
    case maybeIdx of
      Just idx -> idx
      Nothing  -> error "WTF"
  )

cipher text key op = do
  let shiftCycles = take (length text) $ cycle (keyToNumbers key)
  let abcLen      = length alphabet
  zipWith
    (\c i -> do
      let maybeIdx = elemIndex c alphabet
      let idx = case maybeIdx of
            Just idx -> idx
            Nothing  -> error "WTF"
      alphabet !! (((op idx i `rem` abcLen) + abcLen) `rem` abcLen)
    )
    text
    shiftCycles

encrypt plaintext key = cipher plaintext key (+)
decrypt plaintext key = cipher plaintext key (-)

cipherIO t = case t of
  "1" -> encryptIO
  "2" -> decryptIO
  _   -> error "Dude pls!"

encryptIO = do
  putStrLn "Plaintext:"
  plaintext <- getLine
  putStrLn "Key(string):"
  keyStr <- getLine
  putStrLn (encrypt plaintext keyStr)

decryptIO = do
  putStrLn "Ciphertext:"
  plaintext <- getLine
  putStrLn "Key(string):"
  keyStr <- getLine
  putStrLn (decrypt plaintext keyStr)
