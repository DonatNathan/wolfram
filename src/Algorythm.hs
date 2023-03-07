--
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-wolfram-nathan.donat-filliod
-- File description:
-- Algorythm
--

module Algorythm where

import Display

myLoop :: (Int, Int, Int, Int) -> [Char] -> [Char] -> [Char] -> Int -> IO ()
myLoop (window, start, lines, move) (binary) (leftList) (rightList) line = do
    if line == lines then putStr "" else do
        displayLine leftList rightList line (window, start, move)
        let newLeft = createNewLeft binary leftList rightList
        let newRight = createNewRight binary leftList rightList
        myLoop (window, start, lines, move) binary newLeft newRight (line + 1)

createNewLeft :: [Char] -> [Char] -> [Char] -> [Char]
createNewLeft (binary) [] (rightList) = []
createNewLeft (binary) (l:leftList) (rightList) = 
    createNewLeft binary leftList rightList ++ [setValue l l l binary]

createNewRight :: [Char] -> [Char] -> [Char] -> [Char]
createNewRight (binary) (leftList) [] = []
createNewRight (binary) (leftList) (r:rightList) = 
    createNewRight binary leftList rightList ++ [setValue r r r binary]

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
