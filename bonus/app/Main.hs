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
import Parsing (getRule,
                getStart,
                getLines,
                getWindow,
                getMove,
                getCharacter,
                getOpts,
                defaultConf)
import Data.Char (chr)

main :: IO ()
main = do
    args <- getArgs
    case (getOpts (Just defaultConf) args) of
        Nothing -> putStrLn "Invalid line formatting" >>
            exitWith (ExitFailure 84)
        Just conf -> displayLine
                (getFirstLine [(chr (getCharacter conf))]
                (getWindow conf - 1)) (getRule conf)
                (getStart conf) (getLines conf)
                (getMove conf) (getWindow conf)
                (getCharacter conf)
