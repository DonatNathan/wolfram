{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-wolfram-nathan.donat-filliod
-- File description:
-- BinaryName
-}

module BinaryName where

intToBin :: Int -> [Char]
intToBin 0 = []
intToBin a | a `mod` 2 == 1 = intToBin (a `div` 2) ++ ['*']
           | a `mod` 2 == 0 = intToBin (a `div` 2) ++ [' ']

addZero :: [Char] -> [Char]
addZero (list) = 
    if (length list) < 8 then (addZero ((list) ++ [' '])) else (list)
