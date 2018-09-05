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
cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 10 | head -n 50000 > random.test
```

## tests

```bash
$ ./testsort.sh
unix sort
0m0.072s user, 0m0.076s real, 0m0.004s sys

python sort
0m0.039s user, 0m0.043s real, 0m0.005s sys

haskell Data.List sort
0m0.113s user, 0m0.134s real, 0m0.021s sys

haskell Data.Sequence sort
0m0.109s user, 0m0.114s real, 0m0.004s sys

haskell Data.Sequence unstable sort
0m0.099s user, 0m0.108s real, 0m0.009s sys
```

## benchmarks

Using [Criterion](http://hackage.haskell.org/package/criterion) to compare the
Haskell sort functions:

```
hsort-0.6.0: benchmarks  
Running 1 benchmarks...  
Benchmark benchmark: RUNNING...
benchmarking 100/Data.List merge sort
time                 16.15 μs   (16.10 μs .. 16.21 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 16.18 μs   (16.15 μs .. 16.24 μs)
std dev              145.3 ns   (104.6 ns .. 196.9 ns)
                         
benchmarking 100/Data.Sequence stable sort
time                 18.87 μs   (18.81 μs .. 18.92 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 18.88 μs   (18.84 μs .. 18.95 μs)
std dev              173.6 ns   (123.4 ns .. 244.6 ns)
                         
benchmarking 100/Data.Sequence unstable sort
time                 16.23 μs   (16.20 μs .. 16.27 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 16.26 μs   (16.23 μs .. 16.31 μs)
std dev              132.4 ns   (99.43 ns .. 186.5 ns)
                         
benchmarking 1000/Data.List merge sort
time                 308.1 μs   (307.0 μs .. 309.2 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 308.1 μs   (307.5 μs .. 309.0 μs)
std dev              2.528 μs   (1.730 μs .. 3.767 μs)
                         
benchmarking 1000/Data.Sequence stable sort
time                 312.7 μs   (311.4 μs .. 315.2 μs)
                     0.995 R²   (0.989 R² .. 0.999 R²)
mean                 323.3 μs   (316.3 μs .. 337.2 μs)
std dev              31.00 μs   (19.24 μs .. 45.86 μs)
variance introduced by outliers: 76% (severely inflated)
                         
benchmarking 1000/Data.Sequence unstable sort
time                 271.8 μs   (270.4 μs .. 273.4 μs)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 271.5 μs   (270.7 μs .. 273.0 μs)
std dev              3.846 μs   (2.696 μs .. 5.858 μs)
                         
benchmarking 10000/Data.List merge sort
time                 7.360 ms   (7.311 ms .. 7.423 ms)
                     0.999 R²   (0.999 R² .. 1.000 R²)
mean                 7.440 ms   (7.406 ms .. 7.484 ms)
std dev              113.2 μs   (89.13 μs .. 164.1 μs)
                         
benchmarking 10000/Data.Sequence stable sort
time                 6.197 ms   (6.164 ms .. 6.232 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 6.324 ms   (6.285 ms .. 6.387 ms)
std dev              138.0 μs   (94.11 μs .. 216.1 μs)
                         
benchmarking 10000/Data.Sequence unstable sort
time                 5.299 ms   (5.275 ms .. 5.323 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.336 ms   (5.316 ms .. 5.362 ms)
std dev              70.40 μs   (52.52 μs .. 91.09 μs)
                         
Benchmark benchmark: FINISH
```

## References

  - [Haskell High Performance Programming](https://www.packtpub.com/mapt/book/application_development/9781786464217/6/ch06lvl1sec41/reading%252c-writing%252c-and-handling-resources)
