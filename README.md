# hsort

See also [Dokuwiki: Haskell
Examples](http://localhost/dokuwiki/doku.php?id=frank:haskell:examples:sort)

## goal

Find an efficient way to sort text.

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

