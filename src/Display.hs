--
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-wolfram-nathan.donat-filliod
-- File description:
-- Display
--

module Display where 

displayLine :: [Char] -> [Char] -> IO ()
displayLine (leftList) (rightList) = do
    mapM_ putChar (reverse leftList)
    mapM_ putChar rightList
    putChar '\n'
