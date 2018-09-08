{-|
  Module      : Generator
  Description : Random generators for testing.
  Copyright   : © Frank Jung, 2018
  License     : GPL-3
  Maintainer  : frankhjung@linux.com
  Stability   : stable
  Portability : portable
-}

module Generator  ( randomInt
                  , randomWord
                  , randomUpper
                  ) where

import qualified System.Random      (randomRIO)
import qualified Test.RandomStrings as S (onlyAlphaNum, onlyUpper, randomASCII,
                                          randomWord)

-- | Create a random integer between 0 and n.
randomInt :: Int -> IO Int
randomInt n = System.Random.randomRIO (0, n)

-- | Create a random word of size n characters.
randomWord :: Int -> IO String
randomWord = S.randomWord (S.onlyAlphaNum S.randomASCII)

-- | Create a random uppercase words of size n characters.
randomUpper :: Int -> IO String
randomUpper = S.randomWord (S.onlyUpper S.randomASCII)
