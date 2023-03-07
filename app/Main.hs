module Main (main) where

import Lib()
import System.Environment
import Data.List()
import System.Exit(exitWith, ExitCode(ExitFailure))

-- ADD RIGHT STRING FUNCTION
-- addInList :: Char -> [Char] -> [Char]
-- addInList char (list) = (list) ++ [char]

-- DISPLAY LINE

displayLine :: [Char] -> [Char] -> IO ()
displayLine (leftList) (rightList) = do
    mapM_ putChar (reverse leftList)
    mapM_ putChar rightList
    putChar '\n'

-- GET BINARY NAME

intToBin :: Int -> [Char]
intToBin 0 = []
intToBin a | a `mod` 2 == 1 = intToBin (a `div` 2) ++ ['*']
           | a `mod` 2 == 0 = intToBin (a `div` 2) ++ [' ']

addZero :: [Char] -> [Char]
addZero (list) = if (length list) < 8 then (addZero ((list) ++ [' '])) else (list)

-- GAME LOOP

myLoop :: [String] -> [Char] -> [Char] -> [Char] -> Integer -> IO ()
myLoop (args) (binary) (leftList) (rightList) 20 = putStr ""
myLoop (args) (binary) (leftList) (rightList) line = do
    displayLine leftList rightList
    let newLeft = createNewLeft binary leftList rightList
    let newRight = createNewRight binary leftList rightList
    myLoop args binary newLeft newRight (line + 1)

createNewLeft :: [Char] -> [Char] -> [Char] -> [Char]
createNewLeft (binary) [] (rightList) = []
createNewLeft (binary) (l:leftList) (rightList) = createNewLeft binary leftList rightList ++ [setValue l l l binary]

createNewRight :: [Char] -> [Char] -> [Char] -> [Char]
createNewRight (binary) (leftList) [] = []
createNewRight (binary) (leftList) (r:rightList) = createNewRight binary leftList rightList ++ [setValue r r r binary]

-- TODO Remove "!!"
-- SET VALUE FUNCTION
setValue :: Char -> Char -> Char -> [Char] -> Char
setValue ' ' ' ' ' ' (binary_name) = binary_name !! 7
setValue ' ' ' ' '*' (binary_name) = binary_name !! 6
setValue ' ' '*' ' ' (binary_name) = binary_name !! 5
setValue ' ' '*' '*' (binary_name) = binary_name !! 4
setValue '*' ' ' ' ' (binary_name) = binary_name !! 3
setValue '*' ' ' '*' (binary_name) = binary_name !! 2
setValue '*' '*' ' ' (binary_name) = binary_name !! 1
setValue '*' '*' '*' (binary_name) = binary_name !! 0

-- PARSE ARGUMENTS

getRule :: [String] -> Int
getRule [] = -84
getRule (i:j:args) = if i == "--rule" then read j :: Int else getRule args

getWindow :: [String] -> Int
getWindow [] = 80
getWindow (i:j:args) = if i == "--window" then read j :: Int else getWindow args

getStart :: [String] -> Int
getStart [] = 0
getStart (i:j:args) = if i == "--start" then read j :: Int else getStart args

getLines :: [String] -> Int
getLines [] = -1
getLines (i:j:args) = if i == "--lines" then read j :: Int else getLines args

getMove :: [String] -> Int
getMove [] = 0
getMove (i:j:args) = if i == "--move" then read j :: Int else getMove args

isNumber :: [Char] -> Bool
isNumber [] = True
isNumber (i:str) = if i > '9' || i < '0' then False else isNumber str

errorHandlingRule :: Int -> Bool
errorHandlingRule i = if i < 0 || i > 255 then False else True

errorHandlingWindow :: Int -> Bool
errorHandlingWindow i = if i < 0 then False else True

errorHandlingStart :: Int -> Bool
errorHandlingStart i = if i < 0 then False else True

errorHandlingLines :: Int -> Bool
errorHandlingLines i = if i < -1 then False else True

errorHandlingValues :: Int -> Int -> Int -> Int -> Bool
errorHandlingValues rule window start lines = if (errorHandlingRule rule) && (errorHandlingWindow window) && (errorHandlingStart start) && (errorHandlingLines lines) then True else False

errorHandling :: [String] -> [String] -> Bool
errorHandling [] (list) = True
errorHandling (i:j:args) (list) = if (i `elem` list) && isNumber j then errorHandling args list else False

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
            putStrLn binary
            myLoop args binary leftList rightList 0
