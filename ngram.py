#!/usr/bin/python

import array
import bisect
import collections
import itertools
import logging
import logging.handlers
import os
import pickle
import sys
import threading


cache_format_version = 4
cache_dir = os.path.join(os.environ['HOME'], '.cache', 'company-ngram')
log_file = os.path.join(cache_dir, 'ngram.py.log')


def main(argv):
    logging.basicConfig(
        handlers=(
            logging.handlers.RotatingFileHandler(
                log_file,
                maxBytes=10000000,
                backupCount=2,
            ),
        ),
        format='%(asctime)s\t%(levelname)s\t%(message)s',
        level=logging.DEBUG,
    )

    if len(argv) != 3:
        usage_and_exit()
    n = int(argv[1])
    assert n > 1
    data_dir = argv[2]
    tree = []
    def lazy_load():
        tree.extend(load(data_dir, n))
    threading.Thread(target=lazy_load).start()

    stop = lambda : None
    lock = threading.Lock()
    for l in sys.stdin:
        stop()
        words = l.split()
        try:
            n_out_max = int(words[0])
        except:
            exit()
        try:
            timeout = float(words[1])
        except:
            exit()
        results = company_filter(search(tree, words[max(len(words) - (n - 1), 2):], n_out_max))
        stop, lock, dump = make_dump(results, lock)
        threading.Thread(target=dump).start()
        if timeout >= 0:
            threading.Timer(timeout, stop).start()


def usage_and_exit(s=1):
    print(
        """
        echo <query> | {} <n> <data_dir>
        query: n_out_max timeout any words you want to search
        n_out_max: restrict number of candidates
                   no restriction is imposed if n_out_max < 0
        timeout: restrict response time
                 no restrict is imposed if timeout < 0
        """.format(__file__),
        file=sys.stderr,
    )
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


"""
This structure is bit ugly but saves some spaces.
(
    (
        # following entries does not exist if l == 0
        # x > 1
        (c_kx1, c_kx2, ..., c_kxl)
        w_kx1,
        w_kx2,
        ...
        w_kxk,
        # following entries does not exist in leaf nodes
        child_kx1,
        child_kx2,
        ...
        child_kxk,
    ),
    # branches without further branchings
    # following entries does not exist if no such branches are exist
    (w_k11,     w_k12,     ..., w_k1m),
    (w_(k+1)11, w_(k+1)12, ..., w_(k+1)1m),
    ...
)
"""
def make_tree(ngrams):
    if ngrams:
        ngrams.sort()
        return _make_tree(ngrams)
    else:
        return ()


def _make_tree(ngrams):
    if not ngrams:
        return (((),),)
    counts = []
    words = []
    childrens = []
    word1s = []
    children1s = []
    c = 0
    pre = ngrams[0][0]
    children = []
    for ngram in ngrams:
        w = ngram[0]
        if w == pre:
            c += 1
            children.append(ngram[1:])
        else:
            update(
                c, pre, children,
                counts, words, childrens, word1s, children1s,
            )
            pre = w
            c = 1
            children = [ngram[1:]]
    update(
        c, pre, children,
        counts, words, childrens, word1s, children1s,
    )
    return pack_tree(
        counts, words, childrens,
        word1s, children1s,
    )


def pack_tree(
        counts, words, childrens,
        word1s, children1s,
):
    ret = [[]]
    if counts:
        ret[0].append(compress_ints(counts))
        ret[0].extend(words)
        ret[0].extend(childrens)
    ret[0] = tuple(ret[0])
    if word1s:
        ret.append(tuple(word1s))
        ret.extend(zip(*children1s))
    return tuple(ret)


def update(
        c, pre, children,
        counts, words, childrens, word1s, children1s,
):
    #assert c > 0
    if c == 1:
        #assert len(children) == 1
        word1s.append(pre)
        if children[0]:
            children1s.append(children[0])
    else:
        counts.append(c)
        words.append(pre)
        if children[0]:
            childrens.append(_make_tree(children))


def compress_ints(ints):
    n_ints = len(ints)
    if n_ints < 3:
        return tuple(ints)
    else:
        imax = max(ints)
        if imax > 65535:
            return array.array(type_code_of(imax), ints)
        elif imax > 255:
            if n_ints < 4:
                return tuple(ints)
            else:
                return array.array(type_code_of(imax), ints)
        else:
            return array.array(type_code_of(imax), ints)


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
        ret = _search(tree, ngram)
        if n_out_max < 0:
            return ret
        return take(ret, n_out_max)
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


def memoize_candidates(f):
    table = {}
    def memof(tree, ngram):
        ngram = tuple(ngram)
        if None in ngram:
            if ngram in table:
                logging.info('HIT!:\t{}\t{}'.format(len(table[ngram]), ngram))
                return table[ngram]
            else:
                ret = tuple(f(tree, ngram))
                logging.info('set:\t{}\t{}'.format(len(ret), ngram))
                table[ngram] = ret
                return ret
        else:
            ret = f(tree, ngram)
            logging.info('input:\t{}\t{}'.format(len(ret), ngram))
            return ret
    return memof


@memoize_candidates
def candidates(tree, ngram):
    return sorted(
        _candidates(tree, optimize_query(ngram)),
        key=second,
        reverse=True
    )


def _candidates(tree, ngram):
    return merge_candidates(
        itertools.chain(
            _candidates2(tree, ngram),
            _candidates1(tree[1:], ngram)
        )
    )


def _candidates2(tree, ngram):
    cs_ws_children = tree[0]
    if not cs_ws_children:
        return ()
    l = len(cs_ws_children[0])
    if not ngram:
        return zip(cs_ws_children[1:(1 + l)], cs_ws_children[0])

    if len(cs_ws_children) < 1 + 2*l:
        #assert len(cs_ws_children) == 1 + l
        return ()
    #assert len(cs_ws_children) == 1 + 2*l
    w = ngram[0]
    more = ngram[1:]
    if w is None:
        return itertools.chain.from_iterable(
            _candidates(child, more)
            for child
            in cs_ws_children[(l + 1):]
        )
    try:
        i = index(cs_ws_children, w, 1, 1 + l)
    except ValueError:
        return ()
    return _candidates(cs_ws_children[i + l], more)



def _candidates1(wss, ngram):
    if not wss:
        return ()
    if not ngram:
        return zip_with_1(wss[0])
    if ngram[0] is None:
        ngram_more = ngram[1:]
        return itertools.chain.from_iterable(
            match_tuple(ws, ngram_more)
            for ws
            in zip(*wss[1:])
        )
    try:
        i = index(wss[0], ngram[0])
    except ValueError:
        return ()
    return match_tuple([ws[i] for ws in wss[1:]], ngram[1:])



def match_tuple(ws, ngram):
    n = len(ngram)
    if n < len(ws):
        for w, q in zip(ws, ngram):
            if (w != q) and (q is not None):
                return ()
        return ((ws[n], 1),)
    else:
        return ()


def merge_candidates(wcs):
    d = {}
    for w, c in wcs:
        if w in d:
            d[w] += c
        else:
            d[w] = c
    return d.items()


def zip_with_1(xs):
    return zip(xs, (1 for _ in xs))


def optimize_query(ngram):
    i = 0
    for w in ngram:
        if w is None:
            i += 1
        else:
            break
    return ngram[i:]


def index(xs, x, lo=0, hi=None):
    if hi is None:
        hi = len(xs)
    i = bisect.bisect_left(xs, x, lo, hi)
    if i < len(xs) and xs[i] == x:
        return i
    raise ValueError


def make_dump(results, lock_pre):
    stopper = [False]

    def stop():
        stopper[0] = True

    lock_cur = threading.Lock()
    lock_cur.acquire()

    def dump():
        lock_pre.acquire()
        for w, ann in results:
            if stopper[0]:
                break
            print(w + '\t' + ann)
        print()
        print()
        sys.stdout.flush()
        lock_cur.release()
    return stop, lock_cur, dump


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
