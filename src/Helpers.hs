module Helpers where
import Constants

toStr :: Char -> String 
toStr x = [x]

isSpecialChar :: Char -> Bool
isSpecialChar x = isPartOfList x specialChars

isSpecialWord :: String -> Bool
isSpecialWord x = isPartOfList x specialWords

isPartOfList :: Eq a => a -> [a] -> Bool
isPartOfList _ [] = False
isPartOfList item (x:xs)
                | item == x = True
                | otherwise = isPartOfList item xs

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs) 