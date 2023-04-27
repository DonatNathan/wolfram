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
displayString (i:string) number = putChar i >>
    displayString string (number - 1)

displayRString :: [Char] -> Int -> Int -> IO ()
displayRString [] number len = return ()
displayRString (i:string) number len | number >= len = putChar i >> 
    displayRString string (number - 1) (len - 1)
displayRString (i:string) number len | number < len = 
    displayRString string (number) (len - 1)

displayVoid :: Int -> IO ()
displayVoid number | number <= 0 = return ()
displayVoid number = putChar ' ' >>
                     displayVoid (number - 1)

checkSizeWindow :: Int -> Int -> IO ()
checkSizeWindow window line = if window `mod` 2 == 0 then
            displayVoid (window `div` 2 - line - 1)
        else displayVoid (window `div` 2 - line)

displayLine :: [Char] -> [Char] -> Int -> (Int, Int, Int) -> IO ()
displayLine (leftList) (rightList) line (window, start, move) | line < start = 
    return ()
    | line >= start =
        displayVoid (window `div` 2 - line + move) >>
        displayRString (reverse leftList) (window `div` 2) (length leftList) >>
        displayString rightList (window `div` 2) >>
        checkSizeWindow window line >>
        putChar '\n'
