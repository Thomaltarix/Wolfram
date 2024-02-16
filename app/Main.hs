{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Parsing (handleErrors)
import Display (displayWolfram)

main :: IO ()
main = do
    args <- getArgs
    handleErrors args
    return ()
