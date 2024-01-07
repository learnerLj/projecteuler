-- https://projecteuler.net/problem=23
module P23 where

import qualified Data.Set as Set

-- Calculate the final sum
finalSum :: Integer
finalSum = sum [n | n <- [1 .. 28123], not (Set.member n sumsOfTwoAbundants)]
  where
    sumsOfTwoAbundants = Set.fromList [x + y | x <- abundantNums, y <- abundantNums, x >= y, x + y <= 28123]
    sumDivisors n = sum (1 : [x + if x * x == n then 0 else div n x | x <- [2 .. limit], n `mod` x == 0])
      where
        limit = floor . sqrt $ fromIntegral n
    abundantNums = [n | n <- [2 .. 28183], sumDivisors n > n]

main :: IO ()
main = print finalSum

{-
定义：sumAbundant is the number that can be written as the sum of two abundant numbers.

1. not sure whether or not the sum of two sumAbundant is sumAbundant
2. not sure whether a sumAbundant has one or multiple abundant number pairs that compose it
-}
