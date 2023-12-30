-- -- https://projecteuler.net/problem=5

module P5 where

import Criterion.Main
import Data.List (foldl')

lcmRange :: Integer -> Integer
lcmRange n = foldl' lcm 1 [1 .. n]

lcmRangeImproved :: Integer -> Integer
lcmRangeImproved n = product $ map maxPower primesInRange
  where
    primesInRange = takeWhile (<= n) primes
    maxPower p = p ^ maxExponent p
    maxExponent p = last $ takeWhile (\x -> p ^ x <= n) [1 ..]

lcmRangeImproved2 :: Integer -> Integer
lcmRangeImproved2 n = product $ map maxPower primesInRange
  where
    primesInRange = takeWhile (<= n) primes
    maxPower p = p ^ maxExponent n p
    maxExponent n p = if n < p then 0 else 1 + maxExponent (n `div` p) p

primes :: [Integer]
primes = sieve [2 ..]
  where
    sieve [] = []
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- main :: IO ()
-- main = print $ lcmRangeImproved 10

main :: IO ()
main = do
    defaultMain
        [ bgroup "lcmRange" [bench "200000" $ nf lcmRange 200000]
        , bgroup
            "lcmRangeImproved"
            [ bench "200000" $ nf lcmRangeImproved 200000
            ]
        , bgroup
            "lcmRangeImproved2"
            [ bench "200000" $ nf lcmRangeImproved2 200000
            ]
        ]

    print $ lcmRangeImproved 20
