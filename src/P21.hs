module P21 where

import Data.List (group)

sumAcicable :: Integer -> Integer
sumAcicable num = sum $ acicable (num - 1)
  where
    acicable n = [a | a <- [1 .. n], let b = sumDivisors a, a == sumDivisors b, a /= b]
    sumDivisors n = foldr (\(x, y) acc -> acc + x + y) 1 (factors n)
    factors x = [(a, x `div` a) | a <- [2 .. (floor . sqrt $ fromInteger x)], x `mod` a == 0]

-- https://mathschallenge.net/index.php?section=faq&ref=number/sum_of_divisors
sumAcicable2 :: Integer -> Integer
sumAcicable2 num = sum $ acicable (num - 1)
  where
    acicable n = [a | a <- [2 .. n], let b = sumDivisors a, a == sumDivisors b, a /= b]
    sumDivisors n = product deltas - n
      where
        deltas = zipWith (\p a -> (p ^ (a + 1) - 1) `div` (p - 1)) dupFacs exponents
        facs = primeFactors n
        dupFacs = mydup facs
        exponents = map length $ group facs
    mydup [] = []
    mydup [x] = [x]
    mydup (x : xs) = if x == head xs then mydup xs else x : mydup xs

primeFactors :: (Integral a) => a -> [a]
primeFactors m = primeFactors' m primes
  where
    primeFactors' 1 _ = []
    primeFactors' m (p : xs)
      | m `mod` p == 0 = p : primeFactors' (m `div` p) (p : xs)
      | otherwise = primeFactors' m xs
    primes = sieve [2 ..]
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main = print $ sumAcicable 10000
