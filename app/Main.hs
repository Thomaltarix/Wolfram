{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Display (displayLine,
                getFirstLine)
import Parsing (handleErrors,
                getRuleValue,
                getStartValue,
                getLinesValue,
                getWindowSize,
                getMoveValue)

main :: IO ()
main = do
    args <- getArgs
    case handleErrors args of
        Nothing -> exitWith (ExitFailure 84)
        Just conf' -> displayLine
                (getFirstLine "*" (getWindowSize conf' - 1))
                (getRuleValue conf')
                (getStartValue conf')
                (getLinesValue conf')
                (getMoveValue conf')
                (getWindowSize conf')
