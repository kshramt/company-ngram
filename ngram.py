#!/usr/bin/pytyon

import collections
import json
import os
import sys


def query(ngram_tree, words, n_out_max):
    n, children = ngram_tree
    ret = []
    for i in range(n, 0, -1):
        ws = _query(children, words, i, n_out_max)
        n_out_max -= len(ws)
        ret.extend(ws)
        if n_out_max <= 0:
            break
    return ret


def _query(children, words, n, n_out_max):
    words = words[max(len(words) - (n - 1), 0):]
    for word in words:
        n_children = children.get(word, None)
        if n_children is None:
            return []
        _, children = n_children
    ret = []
    i = 0
    for w, (count, _) in sorted(children.items(),
                                key=lambda p: p[1][0],
                                reverse=True):
        if i >= n_out_max:
            break
        ret.append((w, count, n))
        i += 1
    return ret


def make_ngram_tree(n, words):
    assert n > 0
    return (n, _make_ngram_tree(each_cons(words, n)))


def _make_ngram_tree(wordss):
    return {head: (len(more), _make_ngram_tree(more))
            for head, more
            in bundle(wordss).items()}


def bundle(wordss):
    ret = {}
    if wordss[0]:
        for words in wordss:
            w = words[0]
            if w in ret:
                ret[w].append(words[1:])
            else:
                ret[w] = [words[1:]]
    return ret


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
