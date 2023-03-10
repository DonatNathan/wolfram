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

startWolfram :: Int -> Int -> Int -> Int -> Int -> [Char] -> [Char] -> IO ()
startWolfram rule window start lines move leftList rightList = 
    if (errorHandlingValues rule window start lines) == False
    then exitWith (ExitFailure 84) else do
        let binary = (reverse (addZero (reverse (intToBin rule))))
        myLoop (window, start, lines, move) binary leftList rightList 0

main :: IO ()
main = do
    let list = ["--rule", "--window", "--move", "--start", "--lines"]
    args <- getArgs :: IO [String]
    if (errorHandling args list) == False then exitWith (ExitFailure 84) else
        startWolfram (getRule args) (getWindow args) (getStart args) (getLines args) (getMove args) [] ['*']
