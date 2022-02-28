{-# LANGUAGE BangPatterns #-}
module Main where
import qualified Caesar
import qualified Vigenere

main :: IO ()
main = do putStrLn "Pick a cipher"
          putStrLn "1) Caesar's"
          putStrLn "2) Vigenere's"
          cipher <- getLine
          let !cipherFn = case cipher of "1" -> Caesar.cipherIO
                                         "2" -> Vigenere.cipherIO 
                                         _ -> error "Dude pls! cipher"
          putStrLn "1) Encrypt"
          putStrLn "2) Decrypt"
          action <- getLine
          cipherFn action

-- TODO:
-- tests
-- common file - strings, functions
