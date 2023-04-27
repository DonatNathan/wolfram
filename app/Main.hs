{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-wolfram-nathan.donat-filliod
-- File description:
-- Main
-}

module Main (main) where

import ErrorHandling
import GetArgs
import BinaryName
import Algorythm

import System.Environment
import System.Exit

startW :: Int -> Int -> Int -> Int -> Int -> [Char] -> [Char] -> IO ()
startW rule window start lines move leftList rightList = 
    if (errorHandlingValues rule window start lines) == False
    then exitWith (ExitFailure 84) else do
        let binary = (reverse (addZero (reverse (intToBin rule))))
        myLoop (window, start, lines, move) binary leftList rightList 0

main :: IO ()
main = do
    let list = ["--rule", "--window", "--move", "--start", "--lines"]
    a <- getArgs :: IO [String]
    if (errorHandling a list) == False then exitWith (ExitFailure 84)
    else startW (getRule a) (getW a) (getStart a) (getL a) (getMove a) [] ['*']
