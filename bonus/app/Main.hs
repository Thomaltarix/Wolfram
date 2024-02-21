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
    putStrLn "Usage:" >>
    putStr "\t./wolfram --rule r --start s --lines l" >>
    putStrLn "--window w --move m --char c\n" >>
    putStrLn "\tr: rule (30, 90, 110) (Neccessary)" >>
    putStrLn "\ts: start (int) (Optional)" >>
    putStrLn "\tl: lines (int) (Optional)" >>
    putStrLn "\tw: window (int) (Optional)" >>
    putStrLn "\tm: move (int) (Optional)" >>
    putStrLn "\tc: char (visible ascii character integer) (Optional) " >>
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
