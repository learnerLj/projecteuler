-- https://projecteuler.net/problem=4

module P4 (largestPalindromeProduct) where
isPalindrome :: Int -> Bool
isPalindrome n = n == reverseNum n
    where
    reverseNum :: Int -> Int
    reverseNum = go 0
        where
        go acc 0 = acc
        go acc x = let (q, r) = x `quotRem` 10 in go (acc * 10 + r) q


largestPalindromeProduct :: Int -> Int
largestPalindromeProduct n = go 0 upperBound (divisible11 upperBound)
  where
    lowerBound = 10 ^ (n - 1)
    upperBound = 10 ^ n - 1
    go maxPalin x y
      | x < lowerBound = maxPalin
      | y < lowerBound = go maxPalin (x - 1) (divisible11 (x-1))
      | isPalindrome currentPalin = go (max maxPalin currentPalin) (x-1) (divisible11 (x-1))
      | otherwise = go maxPalin x (y - 11)
      where currentPalin = x * y
    divisible11 m = m - m `mod` 11       


main :: IO ()
main = print $ largestPalindromeProduct      3