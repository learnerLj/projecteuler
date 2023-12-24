module P9 () where

findPythagoreanTriplet :: (Num p, Ord p) => p -> Maybe (p, p, p)
findPythagoreanTriplet totalSum = triplet 1 2
  where
    triplet a b
      | c > b, b > a, isPythagorean = Just (a, b, c)
      | b < (c - 1) = triplet a (b + 1)
      | a < (b - 1) = triplet (a + 1) (a+2)
      | otherwise = Nothing
      where
        c = totalSum - a - b
        isPythagorean = a ^ 2 + b ^ 2 == c ^ 2

findPythagoreanTripletImproved :: Integer -> Maybe [(Integer, Integer, Integer)]
findPythagoreanTripletImproved totalSum 
  | odd totalSum = Nothing
  | null candicates = Nothing
  | otherwise = Just $ map getSides candicates
  where
    candicates = let relativePrimeFac (x,y) = (x `div` d, y `div` d,d^2)
                      where d = gcd x y
        in  [relativePrimeFac (a, b) | (a, b) <- factors $ totalSum `div` 2, b < 2 * a]
    getSides (m,k,d) = (d*a,d*b,d*c)
      where 
        n = k-m
        a = m^2 -n^2
        b = 2*m*n
        c = m^2+n^2

factors x = [(a, x `div` a)| a<-[2..(floor . sqrt $ fromInteger x)], x `mod` a ==0 ]



-- (200,375,425)
main = case findPythagoreanTripletImproved 1000 of
    Just triplet -> print triplet
    Nothing -> putStrLn "Not found"