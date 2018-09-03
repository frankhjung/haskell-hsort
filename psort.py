#!/usr/bin/env python

import sys

if __name__ == '__main__':
    rc = 0
    try:
        lines = sys.stdin.readlines()
        lines.sort()
        out = sys.stdout
        map(out.write, lines)
    except:
        rc = 1
    finally:
        out.close()
    sys.exit(rc)
