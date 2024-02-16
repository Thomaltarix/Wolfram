{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Wolfram
-}

module Display (displayLine, getFirstLine) where

getPatternRule30 :: Char -> Char -> Char -> String
getPatternRule30 ' ' ' ' ' ' = " "
getPatternRule30 ' ' ' ' '*' = "*"
getPatternRule30 ' ' '*' ' ' = "*"
getPatternRule30 ' ' '*' '*' = "*"
getPatternRule30 '*' ' ' ' ' = "*"
getPatternRule30 '*' ' ' '*' = " "
getPatternRule30 '*' '*' ' ' = " "
getPatternRule30 '*' '*' '*' = " "
getPatternRule30 _ _ _ = " "

getPatternRule90 :: Char -> Char -> Char -> String
getPatternRule90 ' ' ' ' ' ' = " "
getPatternRule90 ' ' ' ' '*' = "*"
getPatternRule90 ' ' '*' ' ' = " "
getPatternRule90 ' ' '*' '*' = "*"
getPatternRule90 '*' ' ' ' ' = "*"
getPatternRule90 '*' ' ' '*' = " "
getPatternRule90 '*' '*' ' ' = "*"
getPatternRule90 '*' '*' '*' = " "
getPatternRule90 _ _ _ = " "

getPatternRule110 :: Char -> Char -> Char -> String
getPatternRule110 ' ' ' ' ' ' = " "
getPatternRule110 ' ' ' ' '*' = "*"
getPatternRule110 ' ' '*' ' ' = "*"
getPatternRule110 ' ' '*' '*' = "*"
getPatternRule110 '*' ' ' ' ' = " "
getPatternRule110 '*' ' ' '*' = "*"
getPatternRule110 '*' '*' ' ' = "*"
getPatternRule110 '*' '*' '*' = " "
getPatternRule110 _ _ _ = " "

getPattern :: Int -> Char -> Char -> Char -> String
getPattern 30 a b c = getPatternRule30 a b c
getPattern 90 a b c = getPatternRule90 a b c
getPattern 110 a b c = getPatternRule110 a b c
getPattern _ _ _ _ = ""

getNewString :: String -> Int -> String
getNewString [] _ = []
getNewString [x] _ = [x]
getNewString [x, y] _ = [x, y]
getNewString (x:y:z:xs) rule = getPattern rule x y z ++ getNewString (y:z:xs) rule

getFirstLine :: String -> Int -> String
getFirstLine str 0 = str
getFirstLine str n = if n >= 2 then getFirstLine (" " ++ str ++ " ") (n - 2)
                    else getFirstLine (" " ++ str) (n - 1)

printString :: String -> Int -> IO ()
printString [] _ = return ()
printString _ 0 = putStrLn ""
printString string windowSize = do
    let tooLong = (length string - windowSize) `div` 4
    putStrLn (take windowSize (drop tooLong string))

displayStarted :: String -> Int -> Int -> Int -> Int -> Int -> IO ()
displayStarted _ _ _ 0 _ _ = return ()
displayStarted string rule startValue linesValue moveValue windowSize
    | windowSize == 0 = do
        putStrLn ""
        displayLine string rule startValue (linesValue - 1) moveValue windowSize
    | otherwise = do
        printString string windowSize
        let newString = getNewString ("  " ++ string ++ "  ") rule
        displayLine newString rule startValue (linesValue - 1) moveValue windowSize

displayLine :: String -> Int -> Int -> Int -> Int -> Int -> IO ()
displayLine _ _ _ 0 _ _ = return ()
displayLine string rule startValue linesValue moveValue windowSize
    | startValue > 0 = do
        let newString = getNewString ("  " ++ string ++ "  ") rule
        displayLine newString rule (startValue - 1) linesValue moveValue windowSize
    | otherwise = do
        displayStarted string rule startValue linesValue moveValue windowSize

displayWolfram :: Conf -> IO ()
displayWolfram conf = do
    let rule = getRuleValue conf
    let startValue = getStartValue conf
    let linesValue = getLinesValue conf
    let windowSize = getWindowSize conf
    let moveValue = getMoveValue conf
    let string = getFirstLine "*" (windowSize - 1)
    displayLine string rule startValue linesValue moveValue windowSize
