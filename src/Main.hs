module Main(main) where

import qualified Data.List    as DL (sort)
import qualified Data.Text    as DT (lines, unlines)
import qualified Data.Text.IO as DTIO (interact)

main :: IO ()
main = DTIO.interact $ DT.unlines . DL.sort . DT.lines
