module P7 where
import Criterion.Main
import Data.List (foldl')

sieve :: Integral a => [a] -> [a]
sieve [] = []
sieve (p:xs) = p: sieve [x| x<-xs, x `mod` p /=0]

sieveN :: Integral a => a -> [a]
sieveN n = sieve [2..n]

segmentedSieve :: Integer -> [Integer]
segmentedSieve n
  | n < 2     = []
  | otherwise = foldl' (\acc low -> acc ++ sieveSegment low (low + segmentSize) ) [] [2, 2+segmentSize .. n]
  where
    primeSqrtN = sieve [2..segmentSize]
    segmentSize = floor . sqrt . fromIntegral $ n
    sieveSegment low high  = filter isPrime [max low 2..min high n]
      where
        isPrime x = all (\p -> x `mod` p /= 0) $ takeWhile (\p -> p * p <= x) primeSqrtN



main :: IO ()
main = print $ sieve [2..] !! 10000

-- main :: IO ()
-- main = do
--   defaultMain
--     [ bgroup "primes" [bench "primes up to 10000" $ nf (sieveN:: Integer -> [Integer]) 10000],
--       bgroup
--         "segmentedSieve"
--         [ bench "primes up to 10000" $ nf segmentedSieve 10000
--         ]
--     ]