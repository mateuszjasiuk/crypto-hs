module Caesar.IO
  ( cipherIO
  ) where
import           Text.Read

import           Caesar.Core                   as Core

-- TODO: move strings to consts somewhere
cipherIO :: String -> IO ()
cipherIO t = case t of
  "1" -> encryptIO
  "2" -> decryptIO
  _   -> error "Dude pls!"

encryptIO :: IO ()
encryptIO = do
  putStrLn "Plaintext [a-z]:"
  plaintext <- getLine
  putStrLn "Key [Int]:"
  keyStr <- getLine
  let key = case (readMaybe keyStr :: Maybe Int) of
        Just shift -> shift
        Nothing    -> error "nothing"
  print (Core.encrypt (Core.Text plaintext) key)

decryptIO :: IO ()
decryptIO = do
  putStrLn "Ciphertext [a-z]"
  plaintext <- getLine
  putStrLn "Key [Int]:"
  keyStr <- getLine
  let key = case (readMaybe keyStr :: Maybe Int) of
        Just shift -> shift
        Nothing    -> error "nothing"
  print (Core.decrypt (Core.Text plaintext) key)
