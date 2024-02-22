{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Wolfram
-}

module Display (displayLine, getFirstLine, getBinaryValue, isntSpace) where

import Data.Bits(shiftR, shiftL, (.&.))
import Data.Char(chr)

getBinaryValue :: Int -> Int -> Int -> String
getBinaryValue n char index
    | shiftR n index .&. 1 == 0 = " "
    | otherwise = [(chr char)]

isntSpace :: Char -> Bool
isntSpace ' ' = False
isntSpace _ = True

getNumberFromBool :: Bool -> Bool -> Bool -> Int
getNumberFromBool b1 b2 b3 =
    shiftL (fromEnum b1) 2 + shiftL (fromEnum b2) 1 + shiftL (fromEnum b3) 0

getNewStr :: String -> Int -> Int -> String
getNewStr [] _ _ = []
getNewStr [x] _ _ = [x]
getNewStr [x, y] _ _ = [x, y]
getNewStr (x:y:z:xs) rule char =
    (getBinaryValue rule char
    (getNumberFromBool (isntSpace x) (isntSpace y) (isntSpace z)))
    ++ getNewStr (y:z:xs) rule char

getFirstLine :: String -> Int -> String
getFirstLine str 0 = str
getFirstLine str n
    | n >= 2 = getFirstLine (" " ++ str ++ " ") (n - 2)
    | otherwise = getFirstLine (" " ++ str) (n - 1)

printString :: String -> Int -> Int -> IO ()
printString _ _ 0 = putStrLn ""
printString str move size
    | move >= 0 = putStrLn (take size (drop ((length str - size) `div` 4)
    (replicate move ' ' ++ str)))
    | otherwise = putStrLn (take size (drop
        ((length str - size + (-move * 4)) `div` 4)
        (str ++ replicate (-move) ' ')))


displayLine :: String -> Int -> Int -> Int -> Int -> Int -> Int -> IO ()
displayLine _ _ _ 0 _ _ _ = return ()
displayLine string rule 0 line move size char =                  -- Start == 0
    printString string move size >>
    displayLine (getNewStr ("  " ++ string ++ "  ") rule char)
    rule 0 (line - 1) move size char
displayLine string rule start line move size char =              -- Start != 0
    displayLine (getNewStr ("  " ++ string ++ "  ") rule char)
    rule (start - 1) line move size char