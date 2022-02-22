{-# LANGUAGE BangPatterns #-}
module Main where
import qualified Caesar

main :: IO ()
main = do putStrLn "Pick a cipher"
          putStrLn "1) Caesar's"
          cipher <- getLine
          let !cipherFn = case cipher of "1" -> Caesar.cipher
                                         _ -> error "Dude pls! cipher"
          putStrLn "1) Encrypt"
          putStrLn "2) Decrypt"
          action <- getLine
          cipherFn action
