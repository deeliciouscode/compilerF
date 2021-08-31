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

get1 (x,_,_,_,_,_,_) = x
get2 (_,x,_,_,_,_,_) = x
get3 (_,_,x,_,_,_,_) = x
get4 (_,_,_,x,_,_,_) = x
get5 (_,_,_,_,x,_,_) = x
get6 (_,_,_,_,_,x,_) = x
get7 (_,_,_,_,_,_,x) = x