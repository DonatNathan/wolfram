{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-LYN-4-1-wolfram-nathan.donat-filliod
-- File description:
-- ErrorHandling
-}

module ErrorHandling where 

isNumber :: [Char] -> Bool
isNumber [] = True
isNumber (i:str) =
    if (i > '9' || i < '0') && i /= '-' then False else isNumber str

errorHandlingRule :: Int -> Bool
errorHandlingRule i = i >= 0 && i <= 255

errorHandlingWindow :: Int -> Bool
errorHandlingWindow i = i >= 0

errorHandlingStart :: Int -> Bool
errorHandlingStart i = i >= 0

errorHandlingLines :: Int -> Bool
errorHandlingLines i = i >= -1

errorHandlingValues :: Int -> Int -> Int -> Int -> Bool
errorHandlingValues rule window start lines = (errorHandlingRule rule) &&
    (errorHandlingWindow window) && (errorHandlingStart start) && 
    (errorHandlingLines lines)

errorHandling :: [String] -> [String] -> Bool
errorHandling [] (list) = True
errorHandling (i:[]) (list) = False
errorHandling (i:j:args) (list) = if (i `elem` list) && 
    isNumber j then errorHandling args list else False
