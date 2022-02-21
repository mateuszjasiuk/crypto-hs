{-# LANGUAGE BangPatterns #-}
module Lib
    ( someFunc
    ) where
import Data.Char
import Data.List
import Text.Read

alphabet = "abcdefghijklmnopqrstuvwxyz"

shift word amount op = map (shiftSingleChar amount op) word

shiftSingleChar amount op c = do let maybeIdx = elemIndex c alphabet
                                 case maybeIdx of
                                    Just idx -> alphabet!!(op idx amount `rem` length alphabet)
                                    Nothing -> error "WTF" 
                        

someFunc = do putStrLn "Pick a cipher"
              putStrLn "1) Caesar's"
              cipher <- getLine
              let !cipherFn = case cipher of "1" -> caesarIO
                                             _ -> error "Dude pls! cipher"
              putStrLn "1) Encrypt"
              putStrLn "2) Decrypt"
              action <- getLine
              case cipher of "1" -> cipherFn action
                             _ -> error "Dude pls! action"

caesarIO t = case t of "1" -> caesarEncrypt
                       "2" -> caesarDecrypt
                       _ -> error "Dude pls!"

caesarEncrypt = do putStrLn "Plaintext:"
                   plaintext <- getLine
                   putStrLn "Shift amount:"
                   amountStr <- getLine
                   let amount = case (readMaybe amountStr :: Maybe Int) of
                        Just shift -> shift
                        Nothing -> error "nothing"
                   putStrLn (shift plaintext amount (+))

caesarDecrypt = do putStrLn "Ciphertext:"
                   plaintext <- getLine
                   putStrLn "Shift amount:"
                   amountStr <- getLine
                   let amount = case (readMaybe amountStr :: Maybe Int) of
                        Just shift -> shift
                        Nothing -> error "nothing"
                   putStrLn (shift plaintext amount (-))

