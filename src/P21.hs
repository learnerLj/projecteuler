module P21 (sumAcicable, sumAcicable2) where

import Control.Monad.State
import Data.List (group, nub)
import qualified Data.Map as Map

sumAcicable :: Int -> Int
sumAcicable num = sum $ acicable (num - 1)
  where
    acicable n = [a | a <- [2 .. n], let b = sumDivisors a, a == sumDivisors b, a /= b]
    sumDivisors n = foldr (\(x, y) acc -> acc + x + y) 1 (factors n)
    factors x = [(a, if a * a == x then 0 else x `div` a) | a <- [2 .. (floor . sqrt $ fromIntegral x)], x `mod` a == 0]

-- https://mathschallenge.net/index.php?section=faq&ref=number/sum_of_divisors
sumAcicable2 :: Int -> Int
sumAcicable2 num = evalState (acicable (num - 1)) Map.empty
  where
    primes = sieve [2 .. num `div` 2]
    acicable :: Int -> State (Map.Map Int Int) Int
    acicable n = sum <$> mapM evaluateDivisors [2 .. n]

    evaluateDivisors :: Int -> State (Map.Map Int Int) Int
    evaluateDivisors a = do
      b <- sumDivisors a
      bDivSum <- sumDivisors b
      if bDivSum == a && a /= b then return a else return 0

    sumDivisors :: Int -> State (Map.Map Int Int) Int
    sumDivisors n = do
      cache <- get
      case Map.lookup n cache of
        Just s -> return s
        Nothing -> do
          let facs = primeFactors n primes
              exponents = map length $ group facs
              deltas = zipWith (\p a -> (p ^ (a + 1) - 1) `div` (p - 1)) (nub facs) exponents
              s = product deltas - n
          modify (Map.insert n s)
          return s

    primeFactors :: Int -> [Int] -> [Int]
    primeFactors 1 _ = []
    primeFactors _ [] = []
    primeFactors m (p : ps)
      | m `mod` p == 0 = p : primeFactors (m `div` p) (p : ps)
      | otherwise = primeFactors m ps

    sieve :: [Int] -> [Int]
    sieve [] = []
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main = print $ sumAcicable2 10000
