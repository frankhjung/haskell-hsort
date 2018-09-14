# hsort

## goal

Compare a number of different text sort algorithms.

## sort algorithms

* Benchmark using system [sort](http://www.gnu.org/software/coreutils/)
* [Data.List.sort](http://hackage.haskell.org/package/base/docs/Data-List.html#v:sort) (merge sort)
* [Data.Sequence.sort](http://hackage.haskell.org/package/containers-0.6.0.1/docs/src/Data.Sequence.Internal.Sorting.html#sort)
* [Data.Sequence.unstableSort](http://hackage.haskell.org/package/containers-0.6.0.1/docs/src/Data.Sequence.Internal.Sorting.html#unstableSort)

## test data

Generate a random list of strings to sort:

```bash
cat /dev/urandom | tr -dc 'A-Z' | fold -w 10 | head -n 50000 > random.test
```

Or 

```bash
hsort -t 50000 > random.test
```

## tests

```bash
$ ./testsort.sh
unix sort
0m0.105s user, 0m0.105s real, 0m0.000s sys

python sort
0m0.046s user, 0m0.054s real, 0m0.009s sys

haskell Data.List sort
0m0.111s user, 0m0.131s real, 0m0.020s sys

haskell Data.Sequence sort
0m0.104s user, 0m0.114s real, 0m0.010s sys

haskell Data.Sequence unstable sort
0m0.092s user, 0m0.104s real, 0m0.012s sys
```

## benchmarks

Using [Criterion](http://hackage.haskell.org/package/criterion) to compare the
Haskell sort functions. 

The rendered report looks like [this](./benchmark.html.pdf)

The command line output is here:

```
Configuring hsort-0.7.0...
Running 1 benchmarks...
Benchmark benchmark: RUNNING...
benchmarking 100/Data.List merge sort
time                 22.95 μs   (22.90 μs .. 23.00 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 22.96 μs   (22.93 μs .. 23.02 μs)
std dev              149.9 ns   (103.6 ns .. 226.1 ns)

benchmarking 100/Data.Sequence stable sort
time                 26.26 μs   (26.22 μs .. 26.31 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 26.29 μs   (26.24 μs .. 26.42 μs)
std dev              250.6 ns   (120.9 ns .. 506.0 ns)

benchmarking 100/Data.Sequence unstable sort
time                 24.13 μs   (24.08 μs .. 24.17 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 24.14 μs   (24.11 μs .. 24.17 μs)
std dev              98.69 ns   (76.51 ns .. 132.3 ns)

benchmarking 1000/Data.List merge sort
time                 460.8 μs   (459.5 μs .. 462.2 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 460.8 μs   (460.2 μs .. 461.5 μs)
std dev              2.207 μs   (1.653 μs .. 2.912 μs)

benchmarking 1000/Data.Sequence stable sort
time                 488.5 μs   (487.3 μs .. 489.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 488.1 μs   (487.4 μs .. 489.1 μs)
std dev              2.784 μs   (1.855 μs .. 4.250 μs)

benchmarking 1000/Data.Sequence unstable sort
time                 455.5 μs   (454.5 μs .. 456.9 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 455.7 μs   (455.1 μs .. 456.6 μs)
std dev              2.537 μs   (1.851 μs .. 3.603 μs)

benchmarking 10000/Data.List merge sort
time                 11.99 ms   (11.81 ms .. 12.13 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 12.13 ms   (12.04 ms .. 12.47 ms)
std dev              405.7 μs   (123.2 μs .. 804.5 μs)
variance introduced by outliers: 10% (moderately inflated)

benchmarking 10000/Data.Sequence stable sort
time                 10.51 ms   (10.33 ms .. 10.65 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 10.64 ms   (10.54 ms .. 10.95 ms)
std dev              459.4 μs   (128.7 μs .. 939.6 μs)
variance introduced by outliers: 16% (moderately inflated)

benchmarking 10000/Data.Sequence unstable sort
time                 9.463 ms   (9.334 ms .. 9.575 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 9.638 ms   (9.548 ms .. 9.899 ms)
std dev              403.9 μs   (122.9 μs .. 812.0 μs)
variance introduced by outliers: 18% (moderately inflated)

Benchmark benchmark: FINISH
```

## Dependencies

### compiler

```
wanted: ghc-8.4.3
actual: ghc-8.4.3
```

### global-hints

```
ghc: '8.4.3'
bytestring: '0.10.8.2'
unix: '2.7.2.2'
base: '4.11.1.0'
time: '1.8.0.2'
hpc: '0.6.0.3'
filepath: '1.4.2'
process: '1.6.3.0'
array: '0.5.2.0'
integer-gmp: '1.0.2.0'
containers: '0.5.11.0'
ghc-boot: '8.4.3'
binary: '0.8.5.1'
ghc-prim: '0.5.2.0'
ghci: '8.4.3'
rts: '1.0'
terminfo: '0.4.1.1'
transformers: '0.5.5.0'
deepseq: '1.4.3.0'
ghc-boot-th: '8.4.3'
pretty: '1.1.3.6'
template-haskell: '2.13.0.0'
directory: '1.3.1.5'
```  

### locals

```
hsort:
  version: '0.7.0'
```

### stack ls dependencies

```
array 0.5.2.0
base 4.11.1.0
binary 0.8.5.1
bytestring 0.10.8.2
containers 0.5.11.0
deepseq 1.4.3.0
ghc-prim 0.5.2.0
hsort 0.7.0
integer-gmp 1.0.2.0
parseargs 0.2.0.8
random 1.1
random-strings 0.1.1.0
rts 1.0
text 1.2.3.0
time 1.8.0.2
```

## References

  - [Haskell High Performance Programming](https://www.packtpub.com/mapt/book/application_development/9781786464217/6/ch06lvl1sec41/reading%252c-writing%252c-and-handling-resources)
