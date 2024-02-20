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
import Parsing (getRuleValue, getStartValue, getLinesValue, getWindowSize,
                getMoveValue, getOpts, defaultConf, checkRuleSet)

main :: IO ()
main = do
    args <- getArgs
    case (checkRuleSet =<< getOpts (Just defaultConf) args) of
        Nothing -> putStrLn "Invalid line formatting" >>
            exitWith (ExitFailure 84)
        Just conf -> displayLine
                (getFirstLine "*" (getWindowSize conf - 1))
                (getRuleValue conf) (getStartValue conf)
                (getLinesValue conf)(getMoveValue conf)
                (getWindowSize conf)

