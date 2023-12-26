module P12() where
import Data.List (group)

-- https://projecteuler.net/problem=12

primeFactors :: Integer -> [(Integer, Integer)]
primeFactors n =primeFactors' n primes
    where
        primeFactors' 1 _ = []
        primeFactors' _ [] = []
        primeFactors' m (p:xs)
            | m `mod` p ==0 = (p,t): primeFactors' rest xs
            | otherwise = primeFactors' m xs
            where
            divide m p times
                | m `mod` p ==0 = divide (m `div` p) p (times+1)
                | otherwise = (m,times)
            (rest, t) = divide m p 0
        primes = sieve [2..]
        sieve (p:xs) = p: sieve [x| x<-xs, x `mod` p /=0]


firstDivsorsN :: Integer -> Integer
firstDivsorsN n = head $ [num| (num,d)<-triNumWithDividors, d > n]
    where 
        divisors m =  product [x+1| (_,x)<-primeFactors m]
        triNumWithDividors = [(triNum, divisors triNum)| m<-[1..], let triNum =  m*(m+1) `div` 2]




firstDivsorsN2 :: Integral a => Int -> a
firstDivsorsN2 n = head $ [m| m<-triNum, divisorNum m > n]
  where
    divisorNum m = foldl (\acc x-> acc*(x+1)) 1 $ primeExponents m
    primeExponents m = map length $ group $ primeFactors2 m
    primeFactors2 m = primeFactors' m primes
        where
            primeFactors' 1 _ = []
            primeFactors' m (p:xs)
                | m `mod` p == 0 = p : primeFactors' (m `div` p) (p:xs)
                | otherwise = primeFactors' m xs
    primes = sieve [2..]
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    triNum= [m*(m+1) `div` 2| m<-[1..]]

main = print $ firstDivsorsN2 500
        
