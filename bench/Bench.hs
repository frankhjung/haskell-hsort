{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import qualified Generator      (randomUpper)

import qualified Control.Monad  (replicateM)
import           Criterion.Main (Benchmark (..), bench, bgroup, defaultMain,
                                 env, nf)
import qualified Data.List      (sort)
import qualified Data.Sequence  (fromList, sort, unstableSort)

-- | Default word size
wordSize :: Int
wordSize = 10

-- | Benchmarks each sort algorithm.
-- Where:
--  env        = setup test data as list of size n words each of length 10 characters
--  replicateM = perform action n times, collecting results
benchAtSize :: Int -> Benchmark
benchAtSize n =
  env (Control.Monad.replicateM n (Generator.randomUpper wordSize)) $
    \xs ->
      bgroup (show n)
        [
          bench "Data.List merge sort" $ nf Data.List.sort xs
        , bench "Data.Sequence stable sort" $ nf (Data.Sequence.sort . Data.Sequence.fromList) xs
        , bench "Data.Sequence unstable sort" $ nf (Data.Sequence.unstableSort . Data.Sequence.fromList) xs
        ]

-- | Benchmark with different list sizes.
main :: IO ()
main = defaultMain (map benchAtSize [100000])
