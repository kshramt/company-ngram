#!/usr/bin/pytyon

import collections
import json
import os
import sys


def query(ngram_tree, words, n_out_max):
    n, tree = ngram_tree
    ret = []
    for i in range(n, 0, -1):
        ws = _query(tree, words, i, n_out_max)
        n_out_max -= len(ws)
        ret.extend(ws)
        if n_out_max <= 0:
            break
    return ret


def _query(tree, words, n, n_out_max):
    words = tuple(words[max(len(words) - (n - 1), 0):])
    ret = [(w, count, n) for w, count in tree.get(words, [])]
    return ret[:min(len(ret), n_out_max)]


def make_ngram_tree(n, words):
    assert n > 0
    tree = {}
    for i in range(1, n + 1):
        for prevs, nexts in _make_ngram_tree(each_cons(words, i)).items():
            tree[prevs] = tuple(sorted(nexts.items(), key=lambda p: p[1], reverse=True))
    return (n, tree)


def _make_ngram_tree(wordss):
    subtree = {}
    for words in wordss:
        prevs = tuple(words[:-1])
        w = words[-1]
        if prevs in subtree:
            if w in subtree[prevs]:
                subtree[prevs][w] += 1
            else:
                subtree[prevs][w] = 1
        else:
            subtree[prevs] = {w: 1}
    return subtree


def each_cons(xs, n):
    assert n >= 1
    if isinstance(xs, collections.Iterator):
        return _each_cons_iter(xs, n)
    else:
        return _each_cons(xs, n)


def _each_cons(xs, n):
    return [xs[i:i+n] for i in range(len(xs) - (n - 1))]


def _each_cons_iter(xs, n):
    ret = []
    for _ in range(n):
        ret.append(next(xs))
    yield ret
    for x in xs:
        ret = ret[1:]
        ret.append(x)
        yield ret


def read_and_split_all_txt(data_dir):
    ret = []
    for f in os.listdir(data_dir):
        if f.endswith('.txt'):
            with open(os.path.join(data_dir, f)) as fh:
                ret.extend(fh.read().split())
    return ret


def usage_and_exit(s=1):
    print('{} <n> <data_dir> < <query>'.format(__file__), file=sys.stderr)
    exit(s)


def main(argv):
    if len(argv) != 3:
        usage_and_exit()
    n = int(argv[1])
    assert n > 1
    data_dir = argv[2]
    ngram_tree = make_ngram_tree(n, read_and_split_all_txt(data_dir))
    for l in sys.stdin:
        words = l.split()
        try:
            n_out_max = int(words[0])
        except:
            exit()
        json.dump(
            query(ngram_tree, words[1:], n_out_max),
            sys.stdout,
            ensure_ascii=False,
        )
        print()


if __name__ == '__main__':
    main(sys.argv)
