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
generating test data ...

unix sort
0m0.095s user, 0m0.096s real, 0m0.000s sys

python sort
0m0.042s user, 0m0.046s real, 0m0.004s sys

haskell Data.List sort
0m0.128s user, 0m0.139s real, 0m0.012s sys

haskell Data.Sequence sort
0m0.107s user, 0m0.119s real, 0m0.012s sys

haskell Data.Sequence unstable sort
0m0.094s user, 0m0.110s real, 0m0.017s sys
```

## benchmarks

Using [Criterion](http://hackage.haskell.org/package/criterion) to compare the
Haskell sort functions. 

The rendered report looks like [this](./benchmark.html.pdf)

The command line output is here:

```
hsort-0.9.0: benchmarks
Running 1 benchmarks...  
Benchmark benchmark: RUNNING...
benchmarking 100000/Data.List merge sort
time                 331.6 ms   (313.3 ms .. 350.9 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 324.8 ms   (323.3 ms .. 325.9 ms)
std dev              1.677 ms   (0.0 s .. 1.916 ms)
variance introduced by outliers: 19% (moderately inflated)
                         
benchmarking 100000/Data.Sequence stable sort
time                 260.9 ms   (255.6 ms .. 266.8 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 253.8 ms   (250.4 ms .. 255.7 ms)
std dev              3.208 ms   (1.629 ms .. 4.041 ms)
variance introduced by outliers: 16% (moderately inflated)
                         
benchmarking 100000/Data.Sequence unstable sort
time                 243.5 ms   (233.8 ms .. 253.2 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 233.4 ms   (229.3 ms .. 236.9 ms)
std dev              4.798 ms   (4.036 ms .. 5.390 ms)
variance introduced by outliers: 16% (moderately inflated)
                         
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
  version: '0.9.0'
```

### stack ls dependencies

```
array 0.5.2.0
base 4.10.1.0
binary 0.8.5.1
bytestring 0.10.8.2
containers 0.5.11.0
deepseq 1.4.3.0
ghc-prim 0.5.1.1
hsort 0.9.0
integer-gmp 1.0.1.0
parseargs 0.2.0.8
random 1.1
random-strings 0.1.1.0
rts 1.0
text 1.2.3.0
time 1.8.0.2
```

## References

  - [Haskell High Performance Programming](https://www.packtpub.com/mapt/book/application_development/9781786464217/6/ch06lvl1sec41/reading%252c-writing%252c-and-handling-resources)
