{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-wolfram-nathan.donat-filliod
-- File description:
-- Display
-}

module Display where 

displayLine :: [Char] -> [Char] -> Int -> (Int, Int, Int) -> IO ()
displayLine (leftList) (rightList) line (window, start, move) = if line < start then putStr "" else do
    mapM_ putChar (reverse leftList)
    mapM_ putChar rightList
    putChar '\n'
