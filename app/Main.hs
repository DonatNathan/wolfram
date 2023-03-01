module Main (main) where

import Lib()
import System.Environment
import Data.List()

-- SET VALUE FUNCTION
-- setValue :: a -> b -> c -> [binary_name] -> value
-- setValue 0 0 0 (binary_name) = binary_name !! 7
-- setValue 0 0 1 (binary_name) = binary_name !! 6
-- setValue 0 1 0 (binary_name) = binary_name !! 5
-- setValue 0 1 1 (binary_name) = binary_name !! 4
-- setValue 1 0 0 (binary_name) = binary_name !! 3
-- setValue 1 0 1 (binary_name) = binary_name !! 2
-- setValue 1 1 0 (binary_name) = binary_name !! 1
-- setValue 1 1 1 (binary_name) = binary_name !! 0

-- ADD RIGHT STRING FUNCTION
addInList :: Char -> [Char] -> [Char]
addInList char (list) = (list) ++ [char]

-- DISPLAY LINE

displayLine :: [Char] -> [Char] -> IO ()
displayLine (leftList) (rightList) = do
    mapM_ putChar (reverse leftList)
    mapM_ putChar rightList
    putChar '\n'

-- GET BINARY NAME

intToBin :: Int -> [Int]
intToBin 0 = []
intToBin a | a `mod` 2 == 1 = intToBin (a `div` 2) ++ [1]
           | a `mod` 2 == 0 = intToBin (a `div` 2) ++ [0]

addZero :: [Int] -> [Int]
addZero (list) = if (length list) < 8 then (addZero ((list) ++ [0])) else (list)

-- GAME LOOP

myLoop :: [String] -> [Int] -> [Char] -> [Char] -> Integer -> IO ()
myLoop (args) (binary) (leftList) (rightList) 20 = putStr ""
myLoop (args) (binary) (leftList) (rightList) line = do
    displayLine leftList rightList
    let newLeft = createNewLeft binary leftList rightList
    let newRight = createNewRight binary leftList rightList
    myLoop args binary newLeft newRight (line + 1)

createNewLeft :: [Int] -> [Char] -> [Char] -> [Char]
createNewLeft (binary) (leftList) (rightList) = leftList

createNewRight :: [Int] -> [Char] -> [Char] -> [Char]
createNewRight (binary) (leftList) (rightList) = rightList

-- PARSE ARGUMENTS

-- toDict :: [String] -> [(a, b)]
-- toDict (x:xs) = 

main :: IO ()
main = do
    args <- getArgs
    let binary = (reverse (addZero (reverse (intToBin 30))))
    let leftList = []
    let rightList = ['*']
    myLoop args binary leftList rightList 0
