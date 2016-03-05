#!/usr/bin/python

import collections
import json
import os
import sys


def query(ngrams, words, n_out_max):
    ret = []
    for i in range(len(words)):
        ws = _query(ngrams, words[i:], n_out_max)
        n_out_max -= len(ws)
        ret.extend(ws)
        if n_out_max <= 0:
            break
    return ret


def _query(ngrams, words, n_out_max):
    n = len(words) + 1
    ret = [(w, count, n)
           for w, count
           in ngrams.get(n, {}).get(tuple(words), [])]
    return ret[:min(len(ret), n_out_max)]


def make_ngrams(words, n_max, n_min=1):
    assert n_max > 0
    ret = {}
    for n in range(n_min, n_max):
        d = {}
        for prevs, nexts in make_ngram(each_cons(words, n)).items():
            d[prevs] = tuple(sorted(nexts.items(), key=second, reverse=True))
        ret[n] = d
    return ret


def second(xs):
    return xs[1]


def make_ngram(wordss):
    d = {}
    for words in wordss:
        prevs = tuple(words[:-1])
        w = words[-1]
        if prevs in d:
            if w in d[prevs]:
                d[prevs][w] += 1
            else:
                d[prevs][w] = 1
        else:
            d[prevs] = {w: 1}
    return d


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


def uniq(candidates):
    ws = set()
    ret = []
    for candidate in candidates:
        w = candidate[0]
        if w not in ws:
            ws.add(w)
            ret.append(candidate)
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
    ngrams = make_ngrams(read_and_split_all_txt(data_dir), n, 2)
    for l in sys.stdin:
        words = l.split()
        try:
            n_out_max = int(words[0])
        except:
            exit()
        json.dump(
            uniq(query(ngrams, words[1:], n_out_max)),
            sys.stdout,
            ensure_ascii=False,
            separators=(',', ':'),
        )
        print()
        sys.stdout.flush()


if __name__ == '__main__':
    main(sys.argv)
