{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Wolfram
-}

module Display (getBinaryValue,
                getPatternRule,
                getNewStr,
                getFirstLine,
                displayLine) where

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

--                       Move  Window
printString :: String -> Int -> Int -> IO ()
printString _ _ 0 = putStrLn ""
printString str move size
    | move >= 0 = putStrLn (take size (drop ((length str - size) `div` 4)
    (replicate move ' ' ++ str)))
    | otherwise = putStrLn (take size (drop
        ((length str - size + (-move * 4)) `div` 4)
        (str ++ replicate (-move) ' ')))

--                       Rule   Start  Line   Move   Window
displayLine :: String -> Int -> Int -> Int -> Int -> Int -> IO ()
displayLine _ _ _ 0 _ _ = return ()
displayLine string rule 0 line move size =                  -- Start == 0
    printString string move size >>
    displayLine (getNewStr ("  " ++ string ++ "  ") rule)
    rule 0 (line - 1) move size
displayLine string rule start line move size =              -- Start != 0
    displayLine (getNewStr ("  " ++ string ++ "  ") rule)
    rule (start - 1) line move size
