module P1(sumOfMultiples) where

sumOfMultiples :: (Integral a) => a -> a
sumOfMultiples limit = sum [x | x <- [1 .. limit - 1], x `mod` 3 == 0 || x `mod` 5 == 0]

input :: Integer
input=1000
main :: IO ()
main = print $ sumOfMultiples input