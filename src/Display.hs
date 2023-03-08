{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-wolfram-nathan.donat-filliod
-- File description:
-- Display
-}

module Display where 

displayVoid :: Int -> IO ()
displayVoid 0 = putStr ""
displayVoid number = do
    putChar ' '
    displayVoid (number - 1)

displayLine :: [Char] -> [Char] -> Int -> (Int, Int, Int) -> IO ()
displayLine (leftList) (rightList) line (window, start, move) = if line < start then putStr "" else do
    displayVoid (window `div` 2 - (line * 2 - 1) `div` 2 + move)
    mapM_ putChar (reverse leftList)
    mapM_ putChar rightList
    displayVoid (window `div` 2 - (line * 2 - 1) `div` 2)
    putChar '\n'
