{-|
  Module      : QuickSort
  Description : Naive implementation of quicksort.
  Copyright   : © Frank Jung, 2018
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable
-}

module QuickSort  ( sort) where

-- | Naive QuickSort as per
-- <http://wiki.c2.com/?QuickSortInHaskell QuickSort in Haskell>
sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = sort ys ++ [x] ++ sort zs
              where
                ys = [a | a <- xs, a <= x]
                zs = [b | b <- xs, b > x]

