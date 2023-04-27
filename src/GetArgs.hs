{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-wolfram-nathan.donat-filliod
-- File description:
-- GetArgs
-}

module GetArgs where
    
getRule :: [String] -> Int
getRule [] = -84
getRule (i:j:args) = if i == "--rule" then read j :: Int else getRule args

getW :: [String] -> Int
getW [] = 80
getW (i:j:args) = 
    if i == "--window" then read j :: Int else getW args

getStart :: [String] -> Int
getStart [] = 0
getStart (i:j:args) = if i == "--start" then read j :: Int else getStart args

getL :: [String] -> Int
getL [] = -1
getL (i:j:args) = if i == "--lines" then read j :: Int else getL args

getMove :: [String] -> Int
getMove [] = 0
getMove (i:j:args) = if i == "--move" then read j :: Int else getMove args
