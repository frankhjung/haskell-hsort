#!/usr/bin/env python

import sys

if __name__ == '__main__':
    rc = 0
    try:
        lines = sys.stdin.readlines()
        out = sys.stdout
        map(out.write, sorted(lines))
    except:
        rc = 1
    finally:
        out.close()
    sys.exit(rc)
