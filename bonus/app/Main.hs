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
import Parsing (getRule, getStart, getLines, getWindow, getMove,
                getCharacter, getOpts, defaultConf, checkRuleSet)
import Data.Char (chr)

displayHelp :: IO ()
displayHelp = putStrLn "Invalid line formatting\n" >>
    putStrLn
    "Usage: ./wolfram --rule r --start s --lines l --window w --move m" >>
    putStrLn "r: rule (30, 90, 110) (Neccessary)" >>
    putStrLn "s: start (int) (Optional)" >>
    putStrLn "l: lines (int) (Optional)" >>
    putStrLn "w: window (int) (Optional)" >>
    putStrLn "m: move (int) (Optional)" >>
    exitWith (ExitFailure 84)

main :: IO ()
main = do
    args <- getArgs
    case (checkRuleSet =<< getOpts (Just defaultConf) args) of
        Nothing -> displayHelp
        Just conf -> displayLine
                (getFirstLine [(chr (getCharacter conf))]
                (getWindow conf - 1)) (getRule conf)
                (getStart conf) (getLines conf)
                (getMove conf) (getWindow conf)
                (getCharacter conf)
