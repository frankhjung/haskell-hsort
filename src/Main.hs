module Main(main) where

import qualified Data.List    as L (sort)
import qualified Data.Text    as T (lines, unlines)
import qualified Data.Text.IO as O (interact)

main :: IO ()
main = O.interact $ T.unlines . L.sort . T.lines
