-- take int and return highest number that can be calculated by multiplying arbitrary parts of it


highestNumber :: Int -> Int
highestNumber 2 = 1
highestNumber 3 = 2
highestNumber n
            | n_mod_3 == 2 = 3 ^ (n `div` 3) * 2
            | n_mod_3 == 1 = 3 ^ ((n `div` 3) - 1) * 2 ^ 2
            | otherwise = 3 ^ (n `div` 3)
            where n_mod_3 = n `mod` 3


highestNumber2 :: Int -> Int 
highestNumber2 2 = 1
highestNumber2 3 = 2
