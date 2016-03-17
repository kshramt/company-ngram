#!/usr/bin/python


import random
import sys

import ngram as ng


def main(argv):
    t4()
    t3()
    t2()
    t1()


def t4():
    random.seed(42)
    m = 200
    ints = [random.randint(1, 4) for _ in range(m)]
    n = 4
    ngrams = ng.each_cons(ints, n)
    t = ng.make_tree(ngrams)
    candidates = ng.search(t, (1, 2), 10000)
    for x, y in zip(
            candidates,
            [(2, 7, (1, 2)), (1, 4, (1, 2)), (3, 3, (1, 2)), (4, 2, (1, 2))],
    ):
        assert x == y


def t3():
    t = ng.make_tree([(1, 2, 3), (1, 4, 3)])
    assert tuple(ng._candidates(t, (1, None))) == ((3, 2),)


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


def t1():
    random.seed(42)
    m = 200
    ints = [random.randint(1, 4) for _ in range(m)]
    n = 4
    ngrams = ng.each_cons(ints, n)
    t = ng.make_tree(ngrams)
    for ngram in ng.each_cons(ints, n - 1):
        for w, c, _ in list(ng.search(t, ngram, m))[:1]:
            assert count(ngrams, list(ngram) + [w]) == c


def count(ngrams, q):
    c = 0
    for ngram in ngrams:
        if all(x == y for x, y in zip(ngram, q)):
            c += 1
    return c


if __name__ == '__main__':
    main(sys.argv)
