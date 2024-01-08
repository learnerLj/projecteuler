module P12 where

import Control.Monad (filterM)
import Control.Monad.State
import Data.List (group)

-- https://projecteuler.net/problem=12

type PrimeState = State [Int]

firstDivsorsN2 :: Int -> Int
firstDivsorsN2 num = evalState (findFirstDivisor num) (sieve [2 ..])
  where
    findFirstDivisor :: Int -> PrimeState Int
    findFirstDivisor n = head <$> filterM (fmap (> n) <$> divisorNum) triNum

    divisorNum :: Int -> PrimeState Int
    divisorNum m = product <$> (map (succ . length) . group <$> primeFactors m)

    primeFactors :: Int -> PrimeState [Int]
    primeFactors num = gets $ \ps -> factorize num ps
      where
        factorize 1 _ = []
        factorize _ [] = []
        factorize m (p : ps)
          | m `mod` p == 0 = p : factorize (m `div` p) (p : ps)
          | otherwise = factorize m ps

    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    triNum = [m * (m + 1) `div` 2 | m <- [1 ..]]

main = print $ firstDivsorsN2 500
