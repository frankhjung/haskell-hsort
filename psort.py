#!/usr/bin/env python3

"""
Sort lines from STDIN to STDOUT.
"""

import sys

try:
    lines = sys.stdin.readlines()
    for line in sorted(lines):
        print(line, end='')
    sys.exit(0)
except KeyboardInterrupt:
    print("Program interrupted by user.")
    sys.exit(1)
except IOError as e:
    print(f"An error occurred: {e}")
    sys.exit(1)
