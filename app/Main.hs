module Main(main) where

import           Control.Monad      (mapM, replicateM, when, (=<<))
import qualified Data.Foldable      as F (toList)
import qualified Data.List          as L (sort)
import qualified Data.Sequence      as S (fromList, sort, unstableSort)
import qualified Data.Text          as T (lines, unlines)
import qualified Data.Text.IO       as O (interact)
import           Generator          (randomUpper)
import           System.Environment (getArgs)

usage :: String
usage = "usage: hsort [-h|-l|-s|-u|-t int]"

-- process argument and execute requested sort algorithm or test generator 
main :: IO ()
main = do

  args <- getArgs

  case args of
    ["-l"] -> O.interact $ T.unlines . L.sort . T.lines
    ["-s"] -> O.interact $ T.unlines . F.toList . S.sort . S.fromList . T.lines
    ["-u"] -> O.interact $ T.unlines . F.toList . S.unstableSort . S.fromList . T.lines
    ["-t", t] -> mapM_ putStrLn =<< replicateM n (randomUpper 10)
                  where n = read t :: Int
    ["-h"] -> putStrLn usage
    _ -> putStrLn usage

