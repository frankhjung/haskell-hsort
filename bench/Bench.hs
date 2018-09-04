{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import qualified Control.Monad  (replicateM)
import           Criterion.Main (Benchmark (..), bench, bgroup, defaultMain,
                                 env, nf)
import qualified System.Random  (randomRIO)

import qualified Data.List      (sort)
import qualified Data.Sequence  (fromList, sort)

-- | Create a random value between 0 and n
int :: Int -> IO Int
int n = System.Random.randomRIO (0, n)

-- | Run sort benchmarks.
-- Where:
--  env = run benchmarks in environment
--  replicateM = perform action n times collection results
benchAtSize :: Int -> Benchmark
benchAtSize n =
  env (Control.Monad.replicateM n (int n)) $
    \xs ->
      bgroup (show n)
        [
          bench "Data.List"     $ nf Data.List.sort xs
        , bench "Data.Sequence" $ nf (Data.Sequence.sort . Data.Sequence.fromList) xs
        ]

--
-- | Run main for different sized inputs.
main :: IO ()
main = defaultMain (map benchAtSize [100, 1000, 10000])