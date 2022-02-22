module Caesar
    ( cipher
    ) where
import Data.Char
import Data.List
import Text.Read

alphabet = "abcdefghijklmnopqrstuvwxyz"

shift word amount op = map (shiftSingleChar amount op) word

shiftSingleChar amount op c = do
    let maybeIdx = elemIndex c alphabet
    case maybeIdx of
        Just idx -> alphabet!!(op idx amount `rem` length alphabet)
        Nothing -> error "WTF" 

cipher t = case t of "1" -> caesarEncryptIO
                     "2" -> caesarDecryptIO
                     _ -> error "Dude pls!"

caesarEncryptIO = do 
    putStrLn "Plaintext:"
    plaintext <- getLine
    putStrLn "Shift amount:"
    amountStr <- getLine
    let amount = case (readMaybe amountStr :: Maybe Int) of
                    Just shift -> shift
                    Nothing -> error "nothing"
    putStrLn (shift plaintext amount (+))

caesarDecryptIO = do
    putStrLn "Ciphertext:"
    plaintext <- getLine
    putStrLn "Shift amount:"
    amountStr <- getLine
    let amount = case (readMaybe amountStr :: Maybe Int) of
                    Just shift -> shift
                    Nothing -> error "nothing"
    putStrLn (shift plaintext amount (-))

