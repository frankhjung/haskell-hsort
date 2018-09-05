module Main(main) where

import           Control.Monad            (when)
import qualified Data.List                as L (sort)
import qualified Data.Text                as T (lines, unlines)
import qualified Data.Text.IO             as O (interact)

import           System.Console.ParseArgs

-- command line options
data Options =
          FlagHelp
        | FlagList
        | FlagSequence
          deriving (Ord, Eq, Show)

argd :: [ Arg Options ]
argd = [
        Arg {
            argIndex = FlagHelp,
            argName  = Just "help",
            argAbbr  = Just 'h',
            argData  = Nothing,
            argDesc  = "Help"
        },
        Arg {
            argIndex = FlagList,
            argName  = Just "list",
            argAbbr  = Just 'l',
            argData  = Nothing,
            argDesc  = "Use sort from Data.List"
        },
        Arg {
            argIndex = FlagSequence,
            argName  = Just "sequence",
            argAbbr  = Just 's',
            argData  = Nothing,
            argDesc  = "Use sort from Data.Sequence"
        }
       ]

main :: IO ()
main = do

  args <- parseArgsIO ArgsComplete argd

  when (gotArg args FlagList) $
    O.interact $ T.unlines . L.sort . T.lines

  when (gotArg args FlagSequence) $
    putStrLn (argsUsage args)

  when (gotArg args FlagHelp) $
    putStrLn (argsUsage args)

