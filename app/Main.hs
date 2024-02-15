{-
-- EPITECH PROJECT, 2024
-- Wolfram
-- File description:
-- Main
-}

module Main (main) where

import System.Environment (getArgs)
import Parsing (handleErrors)

main :: IO ()
main = do
    args <- getArgs
    handleErrors args
    return ()
