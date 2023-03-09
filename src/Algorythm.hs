{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-wolfram-nathan.donat-filliod
-- File description:
-- Algorythm
-}

module Algorythm where

import Display

myLoop :: (Int, Int, Int, Int) -> [Char] -> [Char] -> [Char] -> Int -> IO ()
myLoop (window, start, lines, move) (binary) (leftList) (rightList) line = do
    if line - start == lines then putStr "" else do
        displayLine leftList rightList line (window, start, move)
        let newLeft = createNewLeft binary leftList rightList True
        let newRight = createNewRight binary leftList rightList True
        myLoop (window, start, lines, move) binary newLeft newRight (line + 1)

-- Change all rightList and leftList by l:rightList and j:leftList

createNewLeft :: [Char] -> [Char] -> [Char] -> Bool -> [Char]
createNewLeft (binary) [] [] first = []
createNewLeft (binary) [] (k:rightList) first | first == True = [setValue ' ' ' ' k binary 0] ++ (createNewLeft binary [] [] False)
createNewLeft (binary) [] (k:rightList) first | first == False = []
createNewLeft (binary) (i:leftList) (k:rightList) first | first == True && leftList == [] = [setValue ' ' i k binary 0] ++ (createNewLeft binary [i] (rightList) False)
createNewLeft (binary) (i:leftList) (k:rightList) first | first == False && leftList == [] = [setValue ' ' ' ' i binary 0] ++ (createNewLeft binary [] (rightList) False)
createNewLeft (binary) (i:j:leftList) (k:rightList) first | first == True = [setValue j i k binary 0] ++ (createNewLeft binary (i:j:leftList) rightList False)
createNewLeft (binary) (i:j:leftList) (rightList) first | first == False && leftList == [] = [setValue ' ' j i binary 0] ++ (createNewLeft binary (j:leftList) rightList False)
createNewLeft (binary) (i:j:leftList) (rightList) first | first == False && leftList /= [] = [setValue (head leftList) j i binary 0] ++ (createNewLeft binary (j:leftList) rightList False)

createNewRight :: [Char] -> [Char] -> [Char] -> Bool -> [Char]
createNewRight (binary) [] [] first = []
createNewRight (binary) (leftList) [] first = []
createNewRight (binary) [] (k:rightList) first | rightList == [] && first == True = [setValue ' ' k ' ' binary 0] ++ (createNewRight binary [] [k] False)
createNewRight (binary) (leftList) (k:rightList) first | rightList == [] && first == False = [setValue k ' ' ' ' binary 0] ++ (createNewRight binary (leftList) [] False)
createNewRight (binary) (i:leftList) (k:l:rightList) first | rightList == [] && first == True = [setValue i k l binary 0] ++ (createNewRight binary [] [k, l] False)
createNewRight (binary) (leftList) (k:l:rightList) first | rightList == [] && first == False = [setValue k l ' ' binary 0] ++ (createNewRight binary (leftList) [l] False)
createNewRight (binary) (i:leftList) (k:l:rightList) first | first == True = [setValue i k l binary 0] ++ (createNewRight binary leftList (k:l:rightList) False)
createNewRight (binary) (leftList) (k:l:rightList) first | first == False && rightList == [] = [setValue k l ' ' binary 0] ++ (createNewRight binary leftList (l:rightList) False)
createNewRight (binary) (leftList) (k:l:rightList) first | first == False && rightList /= [] = [setValue k l (head rightList) binary 0] ++ (createNewRight binary leftList (l:rightList) False)

setValue :: Char -> Char -> Char -> [Char] -> Int -> Char
setValue ' ' ' ' ' ' (i:binary_name) cmpt = if cmpt == 7 then i else setValue ' ' ' ' ' ' binary_name (cmpt + 1)
setValue ' ' ' ' '*' (i:binary_name) cmpt = if cmpt == 6 then i else setValue ' ' ' ' '*' binary_name (cmpt + 1)
setValue ' ' '*' ' ' (i:binary_name) cmpt = if cmpt == 5 then i else setValue ' ' '*' ' ' binary_name (cmpt + 1)
setValue ' ' '*' '*' (i:binary_name) cmpt = if cmpt == 4 then i else setValue ' ' '*' '*' binary_name (cmpt + 1)
setValue '*' ' ' ' ' (i:binary_name) cmpt = if cmpt == 3 then i else setValue '*' ' ' ' ' binary_name (cmpt + 1)
setValue '*' ' ' '*' (i:binary_name) cmpt = if cmpt == 2 then i else setValue '*' ' ' '*' binary_name (cmpt + 1)
setValue '*' '*' ' ' (i:binary_name) cmpt = if cmpt == 1 then i else setValue '*' '*' ' ' binary_name (cmpt + 1)
setValue '*' '*' '*' (i:binary_name) cmpt = if cmpt == 0 then i else setValue '*' '*' '*' binary_name (cmpt + 1)
