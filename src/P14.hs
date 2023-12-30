module P14 () where

import Data.List (foldl', maximumBy)
import qualified Data.Map as Map
import Data.Ord (comparing)

longestUnder :: Integer -> Integer
longestUnder n = fst $ maximumBy (comparing snd) $ zip [1 .. n] (map collatzLen [1 .. (n - 1)])
  where
    collatzLen 1 = 1
    collatzLen start
        | even start = 1 + collatzLen (start `div` 2)
        | otherwise = 1 + collatzLen (3 * start + 1)

longestUnderImproved :: Integer -> Integer
longestUnderImproved m = fst $ maximumBy (comparing snd) $ fst $ foldl' collatzFold ([], Map.empty) [1 .. (m - 1)]
  where
    collatzFold (list, cache) num =
        let (len, newCache) = collatzLenSeq' num cache
         in ((num, len) : list, newCache)

    collatzLenSeq' 1 cache = (1, cache)
    collatzLenSeq' n cache =
        case Map.lookup n cache of
            Just len -> (len, cache)
            Nothing ->
                let (len, newCache) =
                        if even n
                            then collatzLenSeq' (n `div` 2) cache
                            else collatzLenSeq' (3 * n + 1) cache
                 in (1 + len, Map.insert n (1 + len) newCache)

main :: IO ()
main = print $ longestUnderImproved 1000000
