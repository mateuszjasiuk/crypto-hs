{-# LANGUAGE BangPatterns #-}
module Main where
import qualified Caesar.IO                     as CaesarIO
import qualified Vigenere.IO                   as VigenereIO

main :: IO ()
main = do
  putStrLn "Pick a cipher"
  putStrLn "1) Caesar's"
  putStrLn "2) Vigenere's"
  cipher <- getLine
  let !cipherFn = case cipher of
        "1" -> CaesarIO.cipherIO
        "2" -> VigenereIO.cipherIO
        _   -> error "Dude pls! cipher"
  putStrLn "1) Encrypt"
  putStrLn "2) Decrypt"
  action <- getLine
  cipherFn action

-- TODO:
-- figure out when to wrap types / common types?
-- typeclass for ciphers?
-- strings to consts
