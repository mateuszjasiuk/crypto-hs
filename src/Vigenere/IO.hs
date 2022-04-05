module Vigenere.IO
  ( cipherIO
  ) where
import           Vigenere.Core                 as Core

cipherIO t = case t of
  "1" -> encryptIO
  "2" -> decryptIO
  _   -> error "Dude pls!"

encryptIO = do
  putStrLn "Plaintext [a-z]:"
  plaintext <- getLine
  putStrLn "Key [a-z]:"
  keyStr <- getLine
  putStrLn (Core.encrypt plaintext keyStr)

decryptIO = do
  putStrLn "Ciphertext [a-z]:"
  plaintext <- getLine
  putStrLn "Key [a-z]:"
  keyStr <- getLine
  putStrLn (Core.decrypt plaintext keyStr)
