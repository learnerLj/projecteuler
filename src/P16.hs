module P16 where

digitSum :: Integer -> Integer
digitSum e = sum $ digits mySum
    where 
        mySum = 2^e
        digits n
            | n<10 = [n]
            |otherwise = let  (res,remain) = n `quotRem` 10 in remain: digits res
main = print $ digitSum 1000