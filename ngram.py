#!/usr/bin/python

import array
import bisect
import collections
import itertools
import json
import os
import pickle
import sys
import threading


cache_format_version = 3
cache_dir = os.path.join(os.environ['HOME'], '.cache', 'company-ngram')


def main(argv):
    if len(argv) != 3:
        usage_and_exit()
    n = int(argv[1])
    assert n > 1
    data_dir = argv[2]
    # tree = {}
    tree = []
    def lazy_load():
        # tree.update(load(data_dir, n))
        tree.extend(load(data_dir, n))
    threading.Thread(target=lazy_load).start()
    for l in sys.stdin:
        words = l.split()
        try:
            n_out_max = int(words[0])
        except:
            exit()
        dump(company_filter(search(tree, words[max(len(words) - (n - 1), 1):], n_out_max)))
        sys.stdout.flush()


def usage_and_exit(s=1):
    print('{} <n> <data_dir> < <query>'.format(__file__), file=sys.stderr)
    exit(s)


def load(data_dir, n):
    txt_file_names = tuple(os.path.join(data_dir, f) for f in os.listdir(data_dir) if f.endswith('.txt'))
    mtime = max(os.path.getmtime(txt_file_name) for txt_file_name in txt_file_names)
    cache_file = os.path.join(
        cache_dir,
        str(cache_format_version),
        str(n) + os.path.abspath(os.path.dirname(txt_file_names[0])),
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

    tree = make_tree(each_cons(read_and_split_all_txt(txt_file_names), n))

    def save():
        os.makedirs(os.path.dirname(cache_file), exist_ok=True)
        with open(cache_file, 'wb') as fh:
            pickle.dump(tree, fh)
    threading.Thread(target=save).start()

    return tree


def read_and_split_all_txt(file_names):
    words = []
    for f in file_names:
        with open(f) as fh:
            words.extend(sys.intern(w) for w in fh.read().split())
    return words


def make_tree(ngrams):
    if ngrams:
        ngrams.sort()
        return _make_tree(ngrams)
    else:
        return ()


def _make_tree(ngrams):
    words = []
    counts = []
    childrens = []
    pre = ngrams[0][0]
    c = 0
    if len(ngrams[0]) > 1:
        children = []
        for ngram in ngrams:
            w = ngram[0]
            if w == pre:
                c += 1
                children.append(ngram[1:])
            else:
                words.append(pre)
                counts.append(c)
                update_childrens(childrens, children)
                pre = w
                c = 1
                children = [ngram[1:]]
        words.append(pre)
        counts.append(c)
        update_childrens(childrens, children)
        return (
            tuple(words),
            compress_ints(counts),
            tuple(childrens),
        )
    else:
        for ngram in ngrams:
            w = ngram[0]
            if w == pre:
                c += 1
            else:
                words.append(pre)
                counts.append(c)
                pre = w
                c = 1
        words.append(pre)
        counts.append(c)
        return (
            tuple(words),
            compress_ints(counts),
        )


def compress_ints(ints):
    n_ints = len(ints)
    if n_ints < 3:
        return tuple(ints)
    else:
        imax = max(ints)
        if imax > 65535:
            return tuple(ints)
        elif imax > 255:
            if n_ints < 4:
                return tuple(ints)
            else:
                return array.array(type_code_of(imax), ints)
        else:
            return array.array(type_code_of(imax), ints)


def update_childrens(childrens, children):
    childrens.append(_make_tree(children))


def company_filter(wcns):
    for w, c, ngram in wcns:
        yield w, format_ann(c, ngram)


def format_ann(c, ngram):
    return str(c) + format_query(ngram)


def format_query(ngram):
    return '.' + ''.join(map(_format_query, ngram))


def _format_query(w):
    if w is None:
        return '0'
    else:
        return '1'


def search(tree, ngram, n_out_max):
    if tree:
        return take(_search(tree, ngram), n_out_max)
    else:
        return ()


def take(xs, n):
    xs = iter(xs)
    for i in range(n):
        yield next(xs)


def _search(tree, ngram):
    seen = set()
    for ngram in fuzzy_queries(ngram):
        if not all(w is None for w in ngram):
            for w, c in candidates(tree, ngram):
                if w not in seen:
                    yield (w, c, ngram)
                    seen.add(w)


def fuzzy_queries(ngram):
    for q in itertools.product(*[(w, None) for w in reversed(ngram)]):
        yield tuple(reversed(q))


def candidates(tree, ngram):
    return sorted(
        _candidates(tree, optimize_query(ngram)),
        key=second,
        reverse=True
    )


def _candidates(tree, ngram):
    if ngram:
        if len(tree) < 3:
            return ()
        w = ngram[0]
        more = ngram[1:]
        if w is None:
            return itertools.chain.from_iterable(
                _candidates(child, more)
                for child
                in tree[2]
            )
        try:
            i = index(tree[0], w)
        except ValueError:
            return ()
        return candidates(tree[2][i], more)
    else:
        return zip(tree[0], tree[1])


def optimize_query(ngram):
    i = 0
    for w in ngram:
        if w is None:
            i += 1
        else:
            break
    return ngram[i:]


def index(xs, x):
    i = bisect.bisect_left(xs, x)
    if i < len(xs) and xs[i] == x:
        return i
    raise ValueError


def dump(results):
    dump_plain(results)


def dump_plain(results):
    for w, ann in results:
        print(w + '\t' + ann)
    print()
    print()


def dump_json(results):
    json.dump(
        results,
        sys.stdout,
        ensure_ascii=False,
        separators=(',', ':'),
    )
    print()


def type_code_of(n):
    if n < 256:
        return 'B'
    elif n < 65536:
        return 'I'
    elif n < 4294967296:
        return 'L'
    else:
        return 'Q'


def first(x):
    return x[0]


def second(x):
    return x[1]


def each_cons(xs, n):
    assert n >= 1
    if isinstance(xs, collections.Iterator):
        return _each_cons_iter(xs, n)
    else:
        return _each_cons(xs, n)


def _each_cons(xs, n):
    return [tuple(xs[i:i+n]) for i in range(len(xs) - (n - 1))]


def _each_cons_iter(xs, n):
    ret = []
    for _ in range(n):
        ret.append(next(xs))
    yield tuple(ret)
    for x in xs:
        ret = ret[1:]
        ret.append(x)
        yield tuple(ret)


if __name__ == '__main__':
    main(sys.argv)
