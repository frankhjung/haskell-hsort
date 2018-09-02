module Main(main) where

import qualified Data.Sort (sort)

main :: IO ()
main = interact ( unlines . Data.Sort.sort . lines )
