{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-wolfram-nathan.donat-filliod
-- File description:
-- Display
-}

module Display where 

displayString :: [Char] -> Int -> IO ()
displayString [] number = return ()
displayString (string) number | number <= 0 = return ()
displayString (i:string) number = putChar i >> displayString string (number - 1)

displayReverseString :: [Char] -> Int -> Int -> IO ()
displayReverseString [] number len = return ()
displayReverseString (i:string) number len | number >= len = putChar i >> displayReverseString string (number - 1) (len - 1)
displayReverseString (i:string) number len | number < len = displayReverseString string (number) (len - 1)

displayVoid :: Int -> IO ()
displayVoid number | number <= 0 = return ()
displayVoid number = putChar ' ' >> displayVoid (number - 1)

displayLine :: [Char] -> [Char] -> Int -> (Int, Int, Int) -> IO ()
displayLine (leftList) (rightList) line (window, start, move) = if line < start then return () else do
    if window `mod` 2 == 0 then
        displayVoid (window `div` 2 - line - 1 + move)
    else
        displayVoid (window `div` 2 - line + move)
    displayReverseString (reverse leftList) (window `div` 2) (length leftList)
    -- putChar '|'
    displayString rightList (window `div` 2)
    if window `mod` 2 == 0 then
        displayVoid (window `div` 2 - line - 1)
    else
        displayVoid (window `div` 2 - line)
    putChar '\n'
