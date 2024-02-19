{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Wolfram
-}

module Display (displayLine, getFirstLine) where

getPatternRule30 :: Char -> Char -> Char -> String
getPatternRule30 '*' '*' '*' = " "
getPatternRule30 '*' '*' ' ' = " "
getPatternRule30 '*' ' ' '*' = " "
getPatternRule30 '*' ' ' ' ' = "*"
getPatternRule30 ' ' '*' '*' = "*"
getPatternRule30 ' ' '*' ' ' = "*"
getPatternRule30 ' ' ' ' '*' = "*"
getPatternRule30 ' ' ' ' ' ' = " "
getPatternRule30 _ _ _ = " "

getPatternRule90 :: Char -> Char -> Char -> String
getPatternRule90 '*' '*' '*' = " "
getPatternRule90 '*' '*' ' ' = "*"
getPatternRule90 '*' ' ' '*' = " "
getPatternRule90 '*' ' ' ' ' = "*"
getPatternRule90 ' ' '*' '*' = "*"
getPatternRule90 ' ' '*' ' ' = " "
getPatternRule90 ' ' ' ' '*' = "*"
getPatternRule90 ' ' ' ' ' ' = " "
getPatternRule90 _ _ _ = " "

getPatternRule110 :: Char -> Char -> Char -> String
getPatternRule110 '*' '*' '*' = " "
getPatternRule110 '*' '*' ' ' = "*"
getPatternRule110 '*' ' ' '*' = "*"
getPatternRule110 '*' ' ' ' ' = " "
getPatternRule110 ' ' '*' '*' = "*"
getPatternRule110 ' ' '*' ' ' = "*"
getPatternRule110 ' ' ' ' '*' = "*"
getPatternRule110 ' ' ' ' ' ' = " "
getPatternRule110 _ _ _ = " "

getPattern :: Int -> Char -> Char -> Char -> String
getPattern 30 a b c = getPatternRule30 a b c
getPattern 90 a b c = getPatternRule90 a b c
getPattern 110 a b c = getPatternRule110 a b c
getPattern _ _ _ _ = ""

getNewStr :: String -> Int -> String
getNewStr [] _ = []
getNewStr [x] _ = [x]
getNewStr [x, y] _ = [x, y]
getNewStr (x:y:z:xs) rule = getPattern rule x y z ++getNewStr (y:z:xs) rule

getFirstLine :: String -> Int -> String
getFirstLine str 0 = str
getFirstLine str n = if n >= 2 then getFirstLine (" " ++ str ++ " ") (n - 2)
                    else getFirstLine (" " ++ str) (n - 1)

printString :: String -> Int -> IO ()
printString [] _ = return ()
printString _ 0 = putStrLn ""
printString str size =
    putStrLn (take size (drop ((length str - size) `div` 4) str))

displayStarted :: String -> Int -> Int -> Int -> Int -> Int -> IO ()
displayStarted _ _ _ 0 _ _ = return ()
displayStarted string rule start line move 0 =
    putStrLn "" >> displayLine string rule start (line - 1) move 0
displayStarted string rule start line move size =
    let newString = getNewStr ("  " ++ string ++ "  ") rule in
    printString string size >>
    displayStarted newString rule start (line - 1) move size

displayLine :: String -> Int -> Int -> Int -> Int -> Int -> IO ()
displayLine _ _ _ 0 _ _ = return ()
displayLine string rule 0 line move size =
    displayStarted string rule 0 line move size
displayLine string rule start line move size =
    let newString = getNewStr ("  " ++ string ++ "  ") rule in
    displayLine newString rule (start - 1) line move size
