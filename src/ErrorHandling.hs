{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-wolfram-nathan.donat-filliod
-- File description:
-- ErrorHandling
-}

module ErrorHandling where 

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
errorHandlingValues rule window start lines = if (errorHandlingRule rule) &&
    (errorHandlingWindow window) && (errorHandlingStart start) && 
    (errorHandlingLines lines) then True else False

errorHandling :: [String] -> [String] -> Bool
errorHandling [] (list) = True
errorHandling (i:j:args) (list) = if (i `elem` list) && 
    isNumber j then errorHandling args list else False
