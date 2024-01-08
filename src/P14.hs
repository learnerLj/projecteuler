{-# LANGUAGE TupleSections #-}

module P14 (longestUnder2) where

import Control.Monad.State
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

type Cache = Map.Map Int Int

type CollatzState = State Cache

longestUnder2 :: Int -> Int
longestUnder2 m = fst $ maximumBy (comparing snd) $ evalState (mapM collatzPair [1 .. m - 1]) Map.empty
  where
    collatzPair :: Int -> CollatzState (Int, Int)
    collatzPair num = (num,) <$> collatzLen num

    collatzLen :: Int -> CollatzState Int
    collatzLen 1 = return 1
    collatzLen n = do
      cache <- get
      maybe (updateCache n) return (Map.lookup n cache)
      where
        updateCache num = do
          len <- collatzLen $ if even num then num `div` 2 else 3 * num + 1
          modify (Map.insert num (1 + len))
          return (1 + len)

main :: IO ()
main = print $ longestUnder2 1000000
