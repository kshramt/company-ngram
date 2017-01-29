#!/usr/bin/python

import atexit
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


cache_format_version = 7
cache_dir = os.path.join(os.environ['HOME'], '.cache', 'company-ngram')
log_file = os.path.join(cache_dir, 'ngram.py.log')


# -------- main


not_found = -1


def main(argv):
    setup_logging()

    if len(argv) != 3:
        usage_and_exit()
    n = int(argv[1])
    assert n > 1
    data_dir = os.path.realpath(argv[2])
    logging.info('begin:\t{}\t{}'.format(n, data_dir))

    txt_files = txt_files_of(data_dir)
    mtime_max = mtime_max_of(txt_files)

    cache_file = os.path.join(
        cache_dir,
        '{}'.format(cache_format_version),
        '{}'.format(n) + data_dir,
        'cache.pickle',
    )
    cache = {}
    load_cache(
        lambda c: cache.update(c),
        cache_file,
        mtime_max,
    )

    def save_cache():
        try:
            os.makedirs(os.path.dirname(cache_file), exist_ok=True)
            with open(cache_file, 'wb') as fh:
                pickle.dump(cache, fh)
            logging.info('save_cache:\t{}'.format(cache_file))
        except Exception:
            pass
    atexit.register(save_cache)

    db_file = os.path.join(
        cache_dir,
        '{}'.format(cache_format_version),
        '{}'.format(n) + data_dir,
        'db.pickle',
    )
    db = {}

    def lazy_load_db():
        load_db(db, txt_files, n, mtime_max, db_file)
    threading.Thread(target=lazy_load_db).start()

    stop = lambda: None
    for l in sys.stdin:
        stop()
        words = l.split()
        if not words:
            exit()
        if words[0] == 'command':
            if len(words) > 1:
                if words[1] == 'save_cache':
                    save_cache()
            continue
        try:
            n_out_max = int(words[0])
        except Exception:
            exit()
        try:
            timeout = float(words[1])
        except Exception:
            exit()
        results = company_filter(search(
            db,
            tuple(words[max(len(words) - (n - 1), 2):]),
            n_out_max,
            cache,
        ))
        stop, dump = make_dump(results)
        if timeout >= 0:
            threading.Timer(timeout, stop).start()
        dump()


def usage_and_exit(s=1):
    print(
        """
        echo <query> | {} <n> <data_dir>
        query: n_out_max timeout any words you want to search
        n_out_max: restrict number of candidates
                   no restriction is imposed if n_out_max < 0
        timeout: restrict response time
                 no restriction is imposed if timeout < 0
        """.format(__file__),
        file=sys.stderr,
    )
    exit(s)


def setup_logging():
    os.makedirs(os.path.dirname(log_file), exist_ok=True)
    logging.basicConfig(
        handlers=(
            logging.handlers.RotatingFileHandler(
                log_file,
                maxBytes=10000000,
                backupCount=10,
            ),
        ),
        format='%(asctime)s\t%(levelname)s\t%(message)s',
        level=logging.DEBUG,
    )


def make_dump(results):
    stopper = [False]

    def stop():
        stopper[0] = True

    def dump():
        for w, ann in results:
            if stopper[0]:
                break
            print(w, ann, sep='\t')
        end_of_output()
    return stop, dump


def end_of_output():
    print('\n')
    sys.stdout.flush()


# -------- read data


def load_db(
        db,
        txt_files,
        n,
        mtime,
        db_file,
):
    if load_cache(lambda c: db.update(c), db_file, mtime):
        return True

    db.update(make_db(read_and_split_all_txt(txt_files), n))

    def save_db():
        try:
            os.makedirs(os.path.dirname(db_file), exist_ok=True)
            with open(db_file, 'wb') as fh:
                pickle.dump(db, fh)
        except Exception:
            pass
    threading.Thread(target=save_db).start()

    return False


def make_db(ws, n):
    assert n > 1
    sym_of_w, w_of_sym = make_code(ws)
    syms = coding(ws, sym_of_w)
    ngrams = list(each_cons(syms, n))
    ngrams.sort()
    tree = list(range(n))
    tree[0] = tuple(
        array.array(type_code_of(xs[-1]), xs)
        for xs
        in shrink([ngram[0] for ngram in ngrams])
    )
    tree[1:] = [
        array.array(
            type_code_of(len(w_of_sym)),
            [ngram[i] for ngram in ngrams],
        )
        for i
        in range(1, n)
    ]
    return dict(
        tree=tree,
        sym_of_w=sym_of_w,
        w_of_sym=w_of_sym,
    )


def shrink(xs):
    if not xs:
        return (), ()
    ss = []
    ps = []
    pre = xs[0]
    p = -1
    for x in xs:
        if x == pre:
            p += 1
        else:
            ss.append(pre)
            ps.append(p)
            pre = x
            p += 1
    ss.append(pre)
    ps.append(p)
    return ss, ps


def load_cache(f, path, mtime):
    try:
        mtime_cache = os.path.getmtime(path)
    except Exception:
        mtime_cache = -(2**60)
    if mtime_cache > mtime:
        try:
            with open(path, 'rb') as fh:
                f(pickle.load(fh))
            return True
        except Exception:
            pass
    return False


# -------- output formatting


def company_filter(wcns):
    for w, c, ngram in wcns:
        yield w, format_ann(c, ngram)


def format_ann(c, ngram):
    return str(c) + format_query(ngram)


def format_query(ngram):
    return '.' + ''.join(map(_format_query, ngram))


def _format_query(w):
    if w is not_found:
        return '0'
    else:
        return '1'


# -------- search candidates


def search(
        db,
        ws,
        n_out_max,
        cache,
):
    if db:
        ret = _search(db, ws, cache)
        if n_out_max < 0:
            return ret
        return itertools.islice(ret, n_out_max)
    else:
        return ()


def _search(
        db,
        ws,
        cache,
):
    seen = set()
    sym_of_w = db['sym_of_w']
    w_of_sym = db['w_of_sym']
    tree = db['tree']
    for syms in fuzzy_queries(encode(ws, sym_of_w)):
        if all(sym == not_found for sym in syms):
            continue
        if syms in cache:
            logging.info('hit:\t{}\t{}'.format(len(cache[syms]), syms))
            wcs = cache[syms]
        else:
            wcs = tuple((w_of_sym[s], c) for s, c in candidates(tree, syms))
            cache[syms] = wcs
            logging.info('set:\t{}\t{}'.format(len(wcs), syms))
        if not wcs:
            continue
        for w, c in yield_without_dup(wcs, seen):
            yield w, c, syms


def yield_without_dup(wcs, seen):
    for w, c in wcs:
        if w not in seen:
            yield w, c
            seen.add(w)


def candidates(tree, syms):
    assert syms
    assert len(tree) > len(syms)

    assert isinstance(tree[0], tuple)
    syms = optimize_query(syms)
    if not syms:
        return ()
    lo, hi = lo_hi_of(tree[0][0], tree[0][1], syms[0])

    return sorted(
        count_candidates(_candidates(tree[1:], syms[1:], lo, hi)),
        key=lambda x: x[1],
        reverse=True
    )


def _candidates(tree, syms, lo, hi):
    if syms:
        s = syms[0]
        if s is not_found:
            return _candidates_seq(tree, syms, range(lo, hi))
        i1, i2 = range_of(tree[0], s, lo, hi)
        if i2 < i1:
            return ()
        return _candidates(tree[1:], syms[1:], i1, i2)
    else:
        return tree[0][lo:hi]


def _candidates_seq(tree, syms, inds):
    if syms:
        s = syms[0]
        if s is not_found:
            return _candidates_seq(tree[1:], syms[1:], inds)
        t0 = tree[0]
        return _candidates_seq(tree[1:], syms[1:], (i for i in inds if t0[i] == s))
    else:
        t0 = tree[0]
        return (t0[i] for i in inds)


def lo_hi_of(entries, i2s, x):
    """
    - `lo`: inclusive
    - `hi`: exclusive

    Use as `e[lo:hi]`.
    """
    # todo: use interpolation search
    i = bisect.bisect_left(entries, x)
    if entries[i] == x:
        if i == 0:
            return 0, i2s[i] + 1
        else:
            return i2s[i - 1] + 1, i2s[i] + 1
    else:
        return 1, 0


def range_of(xs, y, lo, hi):
    i1 = bisect.bisect_left(xs, y, lo, hi)
    i2 = bisect.bisect_right(xs, y, i1, hi)
    return i1, i2


def count_candidates(ws):
    return collections.Counter(ws).items()


def optimize_query(ws):
    i = 0
    for w in ws:
        if w is not_found:
            i += 1
        else:
            break
    return ws[i:]


def encode(ws, sym_of_w):
    return tuple(_encode(w, sym_of_w) for w in ws)


def _encode(w, sym_of_w):
    if w is not None:
        return sym_of_w.get(w, not_found)


# -------- utilities


def txt_files_of(data_dir):
    try:
        return [
            os.path.join(data_dir, f)
            for f
            in os.listdir(data_dir)
            if f.endswith('.txt')
        ]
    except Exception:
        return []


def mtime_max_of(paths):
    return max(_mtime(path) for path in paths)


def _mtime(path):
    try:
        return os.path.getmtime(path)
    except Exception:
        return 2**60


def read_and_split_all_txt(paths):
    words = []
    for path in paths:
        try:
            with open(path) as fh:
                words.extend(w for w in fh.read().split())
        except Exception:
            pass
    return words


def coding(xs, code):
    return [code[x] for x in xs]


def make_code(ws):
    w_of_sym = sorted(set(ws))
    sym_of_w = dict.fromkeys(w_of_sym)
    for s, w in enumerate(w_of_sym):
        sym_of_w[w] = s
    return sym_of_w, w_of_sym


def make_type_code_of():
    type_codes = ('B', 'H', 'I', 'L')
    base = 2**8
    sizes = tuple(
        base**array.array(t, [0]).itemsize
        for t in type_codes
    )

    def type_code_of(n):
        assert n > -1
        for s, t in zip(sizes, type_codes):
            if n < s:
                return t
        return 'Q'
    return type_code_of


type_code_of = make_type_code_of()


def fuzzy_queries(ws):
    for q in itertools.product(
            *[_query_entry(w)
              for w
              in reversed(ws)]
    ):
        yield tuple(reversed(q))


def _query_entry(w):
    if w == not_found:
        return (not_found,)
    return (w, not_found)


def each_cons(xs, n):
    assert n >= 1
    return _each_cons(xs, n)


def _each_cons(xs, n):
    return [tuple(xs[i:i+n]) for i in range(len(xs) - (n - 1))]


if __name__ == '__main__':
    main(sys.argv)
