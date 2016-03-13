#!/usr/bin/python

import array
import collections
import json
import os
import pickle
import sys
import threading


cache_format_version = 2
cache_dir = os.path.join(os.environ['HOME'], '.cache', 'company-ngram')


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
    ws, cs = ngrams.get(n, {}).get(tuple(words), ((), ()))
    m = len(cs)
    return [(w, c, n) for w, c in zip(ws, cs[:min(m, n_out_max)])]


def make_ngram(words, n):
    d = {}
    for ws in each_cons(words, n):
        prevs = tuple(ws[:-1])
        w = ws[-1]
        if prevs in d:
            if w in d[prevs]:
                d[prevs][w] += 1
            else:
                d[prevs][w] = 1
        else:
            d[prevs] = {w: 1}
    return _make_ngram(d)


def _make_ngram(d):
    ngram = {}
    for prevs, nexts in d.items():
        wcs = sorted(nexts.items(), key=second, reverse=True)
        ws = tuple(wc[0] for wc in wcs)
        cs = array.array('L', [wc[1] for wc in wcs])
        ngram[prevs] = (ws, cs)
    return ngram


def second(xs):
    return xs[1]


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


def read_and_split_all_txt(files):
    ret = []
    for f in files:
        with open(f) as fh:
            ret.extend(sys.intern(w) for w in fh.read().split())
    return ret


def company_filter(candidates):
    buf = {}
    i = 0
    for candidate in candidates:
        w = candidate[0]
        if w in buf:
            buf[w][1] += ' ' + format_count_n(candidate[1:])
        else:
            buf[w] = [i, format_count_n(candidate[1:])]
        i += 1
    return [(w, ann) for w, (_, ann) in sorted(buf.items(), key=lambda kv: kv[1][0])]


def format_count_n(cn):
    return str(cn[0]) + '.'+ str(cn[1])


def usage_and_exit(s=1):
    print('{} <n> <data_dir> < <query>'.format(__file__), file=sys.stderr)
    exit(s)


def load(data_dir, n_max, n_min=1):
    txt_file_names = tuple(os.path.join(data_dir, f) for f in os.listdir(data_dir) if f.endswith('.txt'))
    mtime = max(os.path.getmtime(txt_file_name) for txt_file_name in txt_file_names)
    for n in range(n_min, n_max + 1):
        yield n, _load(txt_file_names, mtime, n)


def _load(txt_file_names, mtime, n):
    cache_file = os.path.join(
        cache_dir,
        str(cache_format_version),
        str(n),
        remove_head_slash(os.path.abspath(os.path.dirname(txt_file_names[0]))),
        'ngram.pickle',
    )
    try:
        mtime_cache_file = os.path.getmtime(cache_file)
    except:
        mtime_cache_file = -(2**60)
    if mtime_cache_file > mtime:
        try:
            with open(cache_file, 'rb') as fh:
                return pickle.load(fh)
        except:
            pass

    ngram = make_ngram(read_and_split_all_txt(txt_file_names), n)

    def save():
        os.makedirs(os.path.dirname(cache_file), exist_ok=True)
        with open(cache_file, 'wb') as fh:
            pickle.dump(ngram, fh)
    threading.Thread(target=save).start()

    return ngram


def memoize(f):
    cache = {}
    def memoized_f(file_names):
        fns = tuple(file_names)
        if fns in cache:
            return cache[fns]
        else:
            return f(fns)
    return memoized_f


@memoize
def read_and_split_all_txt(file_names):
    words = []
    for f in file_names:
        with open(f) as fh:
            words.extend(sys.intern(w) for w in fh.read().split())
    return words


def remove_head_slash(path):
    if path.startswith(os.path.sep):
        return path[1:]
    else:
        return path


def main(argv):
    if len(argv) != 3:
        usage_and_exit()
    n = int(argv[1])
    assert n > 1
    data_dir = argv[2]
    ngrams = {}
    def lazy_load():
        for _n, ngram in load(data_dir, n, 2):
            ngrams[_n] = ngram
    threading.Thread(target=lazy_load).start()
    for l in sys.stdin:
        words = l.split()
        try:
            n_out_max = int(words[0])
        except:
            exit()
        json.dump(
            company_filter(query(ngrams, [sys.intern(w) for w in words[1:]], n_out_max)),
            sys.stdout,
            ensure_ascii=False,
            separators=(',', ':'),
        )
        print()
        sys.stdout.flush()


if __name__ == '__main__':
    main(sys.argv)
