module Main(main) where

import           Control.Monad      (mapM, replicateM, (=<<))
import qualified Data.Foldable      as F (toList)
import qualified Data.List          as L (sort)
import qualified Data.Sequence      as S (fromList, sort, unstableSort)
import qualified Data.Text          as T (lines, unlines)
import qualified Data.Text.IO       as O (interact)
import           Generator          (randomUpper)
import           System.Environment (getArgs)

usage :: String
usage = "usage: hsort [-h|-l|-s|-u|-t int]"

sortText f = O.interact (T.unlines . f . T.lines)

sortSequence f = sortText (F.toList . f . S.fromList)

-- process argument and execute requested sort algorithm or test generator
main :: IO ()
main = do

  args <- getArgs

  case args of
    ["-l"] -> sortText L.sort
    ["-s"] -> sortSequence S.sort
    ["-u"] -> sortSequence S.unstableSort
    ["-t", t] -> mapM_ putStrLn =<< replicateM n (randomUpper 10) where n = read t :: Int
    _ -> putStrLn usage

