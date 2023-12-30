-- -- https://projecteuler.net/problem=2
{-# LANGUAGE NumericUnderscores #-}

module P2 (sumEvenLessThan) where

sumEvenLessThan :: Integer -> Integer
sumEvenLessThan limit = sum $ filter even $ takeWhile (< limit) fibList
  where
    fibList =
        let fibn1 = fibList
            fibn2 = tail fibList
         in 1 : 2 : zipWith (+) fibn1 fibn2

sumEvenLessThanImproved :: Integer -> Integer
sumEvenLessThanImproved limit = sum $ takeWhile (< limit) evenFibList
  where
    evenFibList = 2 : 8 : zipWith (\x y -> x + 4 * y) evenFibn1 evenFibn2
      where
        evenFibn1 = evenFibList
        evenFibn2 = tail evenFibList

input :: Integer
input = 4_000_000

main :: IO ()
main = print $ sumEvenLessThan input
