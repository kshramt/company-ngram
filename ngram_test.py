#!/usr/bin/python


import random
import sys

import ngram as ng


def main(argv):
    t2()


def t2():
    assert tuple(ng.fuzzy_queries((1, 2, 3, 4))) == (
        (1, 2, 3, 4),
        (None, 2, 3, 4),
        (1, None, 3, 4),
        (None, None, 3, 4),
        (1, 2, None, 4),
        (None, 2, None, 4),
        (1, None, None, 4),
        (None, None, None, 4),
        (1, 2, 3, None),
        (None, 2, 3, None),
        (1, None, 3, None),
        (None, None, 3, None),
        (1, 2, None, None),
        (None, 2, None, None),
        (1, None, None, None),
        (None, None, None, None),
    )


if __name__ == '__main__':
    main(sys.argv)
