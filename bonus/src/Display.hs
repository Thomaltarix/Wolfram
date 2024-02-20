{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Wolfram
-}

module Display (displayLine, getFirstLine, getBinaryValue) where

import Data.Bits(shiftR, (.&.))
import Data.Char(chr)

getBinaryValue :: Int -> Int -> Int -> String
getBinaryValue n index char
    | shiftR n index .&. 1 == 0 = " "
    | otherwise = [(chr char)]

getPatternRule :: Int -> Char -> Char -> Char -> Int -> String
getPatternRule n '*' '*' '*' char = getBinaryValue n 7 char
getPatternRule n '*' '*' ' ' char = getBinaryValue n 6 char
getPatternRule n '*' ' ' '*' char = getBinaryValue n 5 char
getPatternRule n '*' ' ' ' ' char = getBinaryValue n 4 char
getPatternRule n ' ' '*' '*' char = getBinaryValue n 3 char
getPatternRule n ' ' '*' ' ' char = getBinaryValue n 2 char
getPatternRule n ' ' ' ' '*' char = getBinaryValue n 1 char
getPatternRule n ' ' ' ' ' ' char = getBinaryValue n 0 char
getPatternRule _ _ _ _ _ = ""

getNewStr :: String -> Int -> Int -> String
getNewStr [] _ _ = []
getNewStr [x] _ _ = [x]
getNewStr [x, y] _ _ = [x, y]
getNewStr (x:y:z:xs) rule char =
    getPatternRule rule x y z char ++ getNewStr (y:z:xs) rule char

getFirstLine :: String -> Int -> String
getFirstLine str 0 = str
getFirstLine str n
    | n >= 2 = getFirstLine (" " ++ str ++ " ") (n - 2)
    | otherwise = getFirstLine (" " ++ str) (n - 1)

getStringFromChar :: Char -> Int -> String
getStringFromChar _ 0 = ""
getStringFromChar c size = c : getStringFromChar c (size - 1)

printString :: String -> Int -> Int -> IO ()
printString [] _ _ = return ()
printString _ 0 _ = putStrLn ""
printString str size move
    | move >= 0 = putStrLn (take size (drop ((length str - size) `div` 4)
        (getStringFromChar ' ' move ++ str)))
    | otherwise = putStrLn (take size (drop
        ((length str - size + (-move * 4)) `div` 4)
        (str ++ getStringFromChar ' ' (-move))))

displayStarted :: String -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
displayStarted _ _ _ 0 _ _ _ = return ()
displayStarted string rule start line move 0 char=
    putStrLn "" >> displayLine string rule start (line - 1) move 0 char
displayStarted string rule start line move size char =
    let newString = getNewStr ("  " ++ string ++ "  ") rule char in
    printString string size move >>
    displayStarted newString rule start (line - 1) move size char

displayLine :: String -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
displayLine _ _ _ 0 _ _ _ = return ()
displayLine string rule 0 line move size char =
    displayStarted string rule 0 line move size char
displayLine string rule start line move size char =
    let newString = getNewStr ("  " ++ string ++ "  ") rule char in
    displayLine newString rule (start - 1) line move size char
