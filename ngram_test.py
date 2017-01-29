#!/usr/bin/python


import random
import sys

import ngram as ng


def main(argv):
    t2()


def t2():
    assert tuple(ng.fuzzy_queries((1, 2, 3, 4))) == (
        (1, 2, 3, 4),
        (ng.not_found, 2, 3, 4),
        (1, ng.not_found, 3, 4),
        (ng.not_found, ng.not_found, 3, 4),
        (1, 2, ng.not_found, 4),
        (ng.not_found, 2, ng.not_found, 4),
        (1, ng.not_found, ng.not_found, 4),
        (ng.not_found, ng.not_found, ng.not_found, 4),
        (1, 2, 3, ng.not_found),
        (ng.not_found, 2, 3, ng.not_found),
        (1, ng.not_found, 3, ng.not_found),
        (ng.not_found, ng.not_found, 3, ng.not_found),
        (1, 2, ng.not_found, ng.not_found),
        (ng.not_found, 2, ng.not_found, ng.not_found),
        (1, ng.not_found, ng.not_found, ng.not_found),
        (ng.not_found, ng.not_found, ng.not_found, ng.not_found),
    )


if __name__ == '__main__':
    main(sys.argv)
