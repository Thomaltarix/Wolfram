{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Wolfram
-}

module Display (displayLine, getFirstLine) where

import Data.Bits(shiftR, (.&.))

getBinaryValue :: Int -> Int -> String
getBinaryValue n index
    | shiftR n index .&. 1 == 0 = " "
    | otherwise = "*"

getPatternRule :: Int -> Char -> Char -> Char -> String
getPatternRule n '*' '*' '*' = getBinaryValue n 7
getPatternRule n '*' '*' ' ' = getBinaryValue n 6
getPatternRule n '*' ' ' '*' = getBinaryValue n 5
getPatternRule n '*' ' ' ' ' = getBinaryValue n 4
getPatternRule n ' ' '*' '*' = getBinaryValue n 3
getPatternRule n ' ' '*' ' ' = getBinaryValue n 2
getPatternRule n ' ' ' ' '*' = getBinaryValue n 1
getPatternRule n ' ' ' ' ' ' = getBinaryValue n 0
getPatternRule _ _ _ _ = ""

getNewStr :: String -> Int -> String
getNewStr [] _ = []
getNewStr [x] _ = [x]
getNewStr [x, y] _ = [x, y]
getNewStr (x:y:z:xs) rule =
    getPatternRule rule x y z ++ getNewStr (y:z:xs) rule

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

displayLine :: String -> Int -> Int -> Int -> Int -> Int -> IO ()
displayLine _ _ _ 0 _ _ = return ()
displayLine string rule 0 line move 0 =                     -- Start == 0 && window == 0
    putStrLn "" >> displayLine string rule 0 (line - 1) move 0
displayLine string rule 0 line move size =                  -- Start == 0
    let newString = getNewStr ("  " ++ string ++ "  ") rule in
    printString string size move >>
    displayLine newString rule 0 (line - 1) move size
displayLine string rule start line move size =              -- Start != 0
    let newString = getNewStr ("  " ++ string ++ "  ") rule in
    displayLine newString rule (start - 1) line move size
