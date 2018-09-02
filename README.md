# hsort - Haskell Sort

## code

The following Haskell will sort from ``STDIN``:

```haskell
module Main(main) where

import qualified Data.Sort (sort)

main = interact ( unlines . Data.Sort.sort . lines )
```

## test results

Generate a random list of strings to sort:

```bash
seq 50000 | xargs -I -- od -vAn -N4 -tx4 /dev/urandom > random.test
```

Benchmark using system [sort](http://www.gnu.org/software/coreutils/):

```bash
$ elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( sort < random.test > random.sorted.test ) 2>&1 1>/dev/null )"; echo $elapsed
0m0.082s user, 0m0.090s real, 0m0.008s sys
```

Execute Haskell version:

```bash
$ elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( hsort < random.test > random.sorted.test ) 2>&1 1>/dev/null )"; echo $elapsed
0m0.207s user, 0m0.232s real, 0m0.025s sys
```

