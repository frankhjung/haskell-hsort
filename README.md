# hsort

See also [Dokuwiki: Haskell
Examples](http://localhost/dokuwiki/doku.php?id=frank:haskell:examples:sort)

## goal

Find an efficient way to sort text.

## sort algorithms

### merge sort

From [Data.List.sort](http://hackage.haskell.org/package/base/docs/Data-List.html#v:sort)


## test data

Generate a random list of strings to sort:

```bash
seq 50000 | xargs -I -- od -vAn -N4 -tx4 /dev/urandom > random.test
```

## test benchmark

Benchmark using system [sort](http://www.gnu.org/software/coreutils/):

```bash
elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( sort < random.test > random.sorted.test ) 2>&1 1>/dev/null )"; echo $elapsed
0m0.076s user, 0m0.089s real, 0m0.013s sys
```

## first naive version

### code

The following Haskell will sort from ``STDIN``:

```haskell
module Main(main) where

import qualified Data.Sort (sort) as DS

main = interact ( unlines . DS.sort . lines )
```

### test

Execute Haskell version:

```bash
elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( hsort < random.test > random.sorted.test ) 2>&1 1>/dev/null )"; echo $elapsed
# 0m0.207s user, 0m0.232s real, 0m0.025s sys
```

## using more efficient Data.Text

Haskell [Strings](https://wiki.haskell.org/Strings) are known to be poor
performers. Instead, use the
[Data.Text](http://hackage.haskell.org/package/text-1.2.3.0/docs/Data-Text.html).

### code

```haskell
module Main(main) where

import qualified Data.List    as DL (sort)
import qualified Data.Text    as DT (lines, unlines)
import qualified Data.Text.IO as DTIO (interact)

main :: IO ()
main = DTIO.interact (DT.unlines . DL.sort . DT.lines)
```

### test

```bash
elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( hsort < random.test > random.sorted.test ) 2>&1 1>/dev/null )"; echo $elapsed
0m0.145s user, 0m0.149s real, 0m0.004s sys
```

# TODO - test the following

Instead of piping straight to `f2`, sort first ...

```haskell
-- file: resourcet.hs

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import System.IO

copy_resourcet :: ResIO ()
copy_resourcet = do
    (_, f1) <- allocate (openFile "read.txt" ReadMode) hClose
    (_, f2) <- allocate (openFile "write.txt" WriteMode) hClose
    liftIO $ hGetContents f1 >>= hPutStr f2
```

Required packages:

  - `resourcet` for `Control.Monad.Trans.Resource`
  - `transformers` for `Control.Monad.IO.Class`

References:

  - [Haskell High Performance Programming](https://www.packtpub.com/mapt/book/application_development/9781786464217/6/ch06lvl1sec41/reading%252c-writing%252c-and-handling-resources)
  - [hGetContents](http://hackage.haskell.org/package/text-1.2.3.0/docs/Data-Text-Lazy-IO.html#v:hGetContents)
