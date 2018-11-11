#!/usr/bin/env bash

# generate random data

# hexadecimals
# seq 50000 | xargs -I -- od -vAn -N4 -tx4 /dev/urandom > random.test
# alphanumerics
# cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 10 | head -n 50000 > random.test
# upper-case alphabetical
echo generating test data ...
hsort -t 50000 > random.test
rm -f random.bench
rm -f random.sorted
echo

# bench mark sort(1) command line utility
echo unix sort
elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( sort -d < random.test > random.bench ) 2>&1 1>/dev/null )"; echo $elapsed
echo

# bench mark python sort
echo python sort
elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( ./psort.py < random.test > random.sorted ) 2>&1 1>/dev/null )"; echo $elapsed
[[ $(cmp random.bench random.sorted) ]] && echo sort failed
echo

# bench mark haskell merge sort
echo haskell Data.List sort
elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( hsort -l < random.test > random.sorted ) 2>&1 1>/dev/null )"; echo $elapsed
[[ $(cmp random.bench random.sorted) ]] && echo sort failed
echo

# bench mark haskell naive quick sort
echo haskell Data.List quicksort
elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( hsort -q < random.test > random.sorted ) 2>&1 1>/dev/null )"; echo $elapsed
[[ $(cmp random.bench random.sorted) ]] && echo sort failed
echo

# bench mark haskell sequence sort
echo haskell Data.Sequence sort
elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( hsort -s < random.test > random.sorted ) 2>&1 1>/dev/null )"; echo $elapsed
[[ $(cmp random.bench random.sorted) ]] && echo sort failed
echo

# bench mark haskell sequence sort
echo haskell Data.Sequence unstable sort
elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( hsort -u < random.test > random.sorted ) 2>&1 1>/dev/null )"; echo $elapsed
[[ $(cmp random.bench random.sorted) ]] && echo sort failed
echo

#EOF
