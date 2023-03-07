{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-wolfram-nathan.donat-filliod
-- File description:
-- Main
-}

module Main (main) where

import ErrorHandling
import GetArgs
import Display
import BinaryName
import Algorythm

import System.Environment
import System.Exit

-- ADD RIGHT STRING FUNCTION
-- addInList :: Char -> [Char] -> [Char]
-- addInList char (list) = (list) ++ [char]

main :: IO ()
main = do
    let list = ["--rule", "--window", "--move", "--start", "--lines"]
    args <- getArgs :: IO [String]
    if (errorHandling args list) == False then exitWith (ExitFailure 84) else do
        let leftList = []
        let rightList = ['*']
        let rule = getRule args
        let window = getWindow args
        let start = getStart args
        let lines = getLines args
        let move = getMove args
        if (errorHandlingValues rule window start lines) == False then exitWith (ExitFailure 84) else do
            let binary = (reverse (addZero (reverse (intToBin rule))))
            myLoop args binary leftList rightList 0
