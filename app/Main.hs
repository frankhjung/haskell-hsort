module Main(main) where

import           Control.Monad          (mapM_, replicateM, (=<<))
import qualified Data.Foldable          as F (toList)
import qualified Data.List              as L (sort)
import qualified Data.Sequence          as S (fromList, sort, unstableSort)
import qualified Data.Sequence.Internal as SI (Seq (..))
import qualified Data.Text              as T (lines, unlines)
import qualified Data.Text.Internal     as TI (Text (..))
import qualified Data.Text.IO           as O (interact)
import           Data.Version           (showVersion)
import           Generator              (randomUpper)
import           Paths_hsort            (version)
import qualified QuickSort              as Q (sort)
import           System.Environment     (getArgs)

usage :: [String]
usage = [ "usage: hsort [-h|-l|-q|-s|-u|-t int]"
        , "Version: " ++ showVersion version
        ]

sortText :: ([TI.Text] -> [TI.Text]) -> IO ()
sortText f = O.interact (T.unlines . f . T.lines)

sortSequence :: Foldable t => (SI.Seq TI.Text -> t TI.Text) -> IO ()
sortSequence f = sortText (F.toList . f . S.fromList)

-- | Process commandline arguments and execute requested sort algorithm
-- or test generator
main :: IO ()
main = do

  args <- getArgs

  case args of
    ["-l"]    -> sortText L.sort
    ["-q"]    -> sortText Q.sort
    ["-s"]    -> sortSequence S.sort
    ["-u"]    -> sortSequence S.unstableSort
    ["-t", t] -> mapM_ putStrLn =<< replicateM n (randomUpper 10) where n = read t :: Int
    _         -> putStrLn $ unlines usage

