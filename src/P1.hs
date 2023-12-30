-- -- https://projecteuler.net/problem=1

module P1 (sumOfMultiples) where

sumOfMultiples :: (Integral a) => a -> a
sumOfMultiples limit = sum [x | x <- [1 .. limit - 1], x `mod` 3 == 0 || x `mod` 5 == 0]

sumOfMultiplesImproved :: (Integral a) => a -> a
sumOfMultiplesImproved limit = sumDivisible3 + sumDivisible5 - sumDivisible15
  where
    cumulativeSum n = n * (n + 1) `div` 2
    limitDivN n = limit `div` n
    sumDivisible n = cumulativeSum $ limitDivN n
    sumDivisible3 = 3 * sumDivisible 3
    sumDivisible5 = 5 * sumDivisible 5
    sumDivisible15 = 15 * sumDivisible 15

input :: Integer
input = 1000

main :: IO ()
main = print $ sumOfMultiples input
