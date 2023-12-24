{-# LANGUAGE NumericUnderscores #-}

module P3 (largestPrimeFactor) where

primes :: [Integer]
primes = sieve [2..]
    where
    sieve [] = []
    sieve (p:xs) = p: sieve [x| x<-xs, x `mod` p /=0]

primeFactors :: Integer -> [Integer]
primeFactors n =primeFactors' n primes
    where
        primeFactors' 1 _ = []
        primeFactors' _ [] = []
        primeFactors' m (p:xs)
            | m `mod` p ==0 = p: primeFactors' (divide m p) xs
            | otherwise = primeFactors' m xs
        divide m p
            | m `mod` p ==0 = divide (m `div` p) p
            | otherwise = m

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n= last $ primeFactors n

main :: IO ()
main = print $ largestPrimeFactor 600_851_475_143