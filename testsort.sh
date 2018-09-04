#!/usr/bin/env bash

function generate {
  # generate test data
  # hexadecimals
  # seq 50000 | xargs -I -- od -vAn -N4 -tx4 /dev/urandom > random.test
  # alphanumerics
  cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w 10 | head -n 50000 > random.test
}

# bench mark sort(1) command line utility
echo unix sort
elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( sort < random.test > random.sorted.test ) 2>&1 1>/dev/null )"; echo $elapsed

# bench mark python sort
echo python sort
elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( ./psort.py < random.test > random.sorted.test ) 2>&1 1>/dev/null )"; echo $elapsed

# bench mark haskell merge sort
echo haskell sort
elapsed="$( TIMEFORMAT='%lU user, %lE real, %lS sys';time ( hsort < random.test > random.sorted.test ) 2>&1 1>/dev/null )"; echo $elapsed

