#!/usr/bin/python

import atexit
import array
import bisect
import itertools
import logging
import logging.handlers
import os
import pickle
import sys
import threading
from typing import (
    Any,
    Callable,
    Dict,
    Iterable,
    Iterator,
    List,
    Mapping,
    Optional,
    Sequence,
    Set,
    Tuple,
    TypeVar,
)


T = TypeVar('T')
T1 = TypeVar('T1')
T2 = TypeVar('T2')
K = TypeVar('K')
V = TypeVar('V')

cache_format_version = 5
cache_dir = os.path.join(os.environ['HOME'], '.cache', 'company-ngram')
log_file = os.path.join(cache_dir, 'ngram.py.log')


# -------- main


def main(argv: List[str]) -> Any:
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
    cache = {} # type: Dict[Tuple[Optional[str], ...], Any]
    load_cache(
        lambda c: cache.update(c),
        cache_file,
        mtime_max,
    )

    def save_cache() -> Any:
        os.makedirs(os.path.dirname(cache_file), exist_ok=True)
        with open(cache_file, 'wb') as fh:
            pickle.dump(cache, fh)
        logging.info('save_cache:\t{}'.format(cache_file))
    atexit.register(save_cache)

    db_file = os.path.join(
        cache_dir,
        '{}'.format(cache_format_version),
        '{}'.format(n) + data_dir,
        'db.pickle',
    )
    db = {} # type: Dict[str, Any]

    def lazy_load_db() -> Any:
        load_db(db, txt_files, n, mtime_max, db_file)
    threading.Thread(target=lazy_load_db).start()

    stop = lambda: None
    not_found = -1
    for l in sys.stdin:
        stop()
        words = l.split()
        if not words:
            exit()
        if words[0] == 'save_cache':
            save_cache()
            continue
        try:
            n_out_max = int(words[0])
        except:
            exit()
        try:
            timeout = float(words[1])
        except:
            exit()
        results = company_filter(search(
            db,
            tuple(words[max(len(words) - (n - 1), 2):]),
            n_out_max,
            cache,
            not_found,
        ))
        stop, dump = make_dump(results)
        if timeout >= 0:
            threading.Timer(timeout, stop).start()
        dump()


def usage_and_exit(s: int=1) -> Any:
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


def setup_logging() -> Any:
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


# def make_dump(results: Iterable[Tuple[str, str]]) -> Tuple[Callable[[], Any], Callable[[], Any]]:
def make_dump(results: Iterable[Tuple[str, str]]) -> Tuple[Callable[[], Any], Callable[[], Any]]:
    stopper = [False]
    dumped = [False]

    def stop() -> Any:
        stopper[0] = True
        if not dumped[0]:
            end_of_output()

    def dump() -> Any:
        stopped_by_stopper = False
        for w, ann in results:
            stopped_by_stopper = stopper[0]
            if stopped_by_stopper:
                break
            print(w, ann, sep='\t')
        dumped[0] = True
        if not stopped_by_stopper:
            end_of_output()
    return stop, dump


def end_of_output():
    print('\n')
    sys.stdout.flush()


# -------- read data


def load_db(
        db: Dict[str, Any],
        txt_files: Sequence[str],
        n: int,
        mtime: int,
        db_file: str,
) -> bool:
    if load_cache(lambda c: db.update(c), db_file, mtime):
        return True

    db.update(make_db(read_and_split_all_txt(txt_files), n))

    def save_db() -> Any:
        os.makedirs(os.path.dirname(db_file), exist_ok=True)
        with open(db_file, 'wb') as fh:
            pickle.dump(db, fh)
    threading.Thread(target=save_db).start()

    return False


def make_db(ws: List[str], n: int) -> Dict[str, Any]:
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


def shrink(xs: Sequence[int]) -> Tuple[Sequence[int], Sequence[int]]:
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


def load_cache(f: Callable[[Any], Any], path: str, mtime: int) -> bool:
    try:
        mtime_cache = os.path.getmtime(path)
    except:
        mtime_cache = -(2**60)
    if mtime_cache > mtime:
        try:
            with open(path, 'rb') as fh:
                f(pickle.load(fh))
            return True
        except:
            pass
    return False


# -------- output formatting


def company_filter(wcns: Iterable[Tuple[str, int, Tuple[Optional[str], ...]]]) -> Iterable[Tuple[str, str]]:
    for w, c, ngram in wcns:
        yield w, format_ann(c, ngram)


def format_ann(c: int, ngram: Tuple[Optional[str], ...]) -> str:
    return str(c) + format_query(ngram)


def format_query(ngram: Iterable[Optional[str]]) -> str:
    return '.' + ''.join(map(_format_query, ngram))


def _format_query(w: Optional[str]) -> str:
    if w is None:
        return '0'
    else:
        return '1'


# -------- search candidates


def search(
        db: Dict[str, Any],
        ws: Tuple[str, ...],
        n_out_max: int,
        cache: Dict[Tuple[Optional[str], ...],
                    Sequence[Tuple[str, int]]],
        not_found: int,
) -> Iterable[Tuple[str, int, Tuple[Optional[str], ...]]]:
    if db:
        ret = _search(db, ws, cache, not_found)
        if n_out_max < 0:
            return ret
        return itertools.islice(ret, n_out_max)
    else:
        return ()


def _search(
        db: Dict[str, Any],
        ws: Tuple[str, ...],
        cache: Dict[Tuple[Optional[str], ...],
                    Sequence[Tuple[str, int]]],
        not_found: int,
) -> Iterable[Tuple[str, int, Tuple[Optional[str], ...]]]:
    seen = set() # type: Set[str]
    sym_of_w = db['sym_of_w']
    w_of_sym = db['w_of_sym']
    tree = db['tree']
    for ws in fuzzy_queries(ws):
        if all(w is None for w in ws):
            continue
        if ws in cache:
            logging.info('hit:\t{}\t{}'.format(len(cache[ws]), ws))
            wcs = cache[ws]
        else:
            syms = encode(ws, sym_of_w, not_found)
            if not_found in syms:
                cache[ws] = ()
                continue
            wcs = tuple((w_of_sym[s], c) for s, c in candidates(tree, syms))
            cache[ws] = wcs
            logging.info('set:\t{}\t{}'.format(len(wcs), ws))
        for w, c in yield_without_dup(wcs, seen):
            yield w, c, ws


def yield_without_dup(wcs: Iterable[Tuple[T1, T2]], seen: Set[T1]) -> Iterator[Tuple[T1, T2]]:
    for w, c in wcs:
        if w not in seen:
            yield w, c
            seen.add(w)


def candidates(tree: List[Sequence[int]], syms: Tuple[Optional[int], ...]) -> Sequence[Tuple[int, int]]:
    assert syms
    assert len(tree) > len(syms)

    assert isinstance(tree[0], tuple)
    syms = optimize_query(syms)
    if not syms:
        return ()
    lo, hi = lo_hi_of(tree[0][0], tree[0][1], syms[0])

    return sorted(
        count_candidates(zip_with_1(_candidates(tree[1:], syms[1:], lo, hi))),
        key=lambda x: x[1],
        reverse=True
    )


def _candidates(tree: List[Sequence[int]], syms: Tuple[int, ...], lo: int, hi: int) -> Iterable[int]:
    if syms:
        s = syms[0]
        if s is None:
            return _candidates_seq(tree, syms, range(lo, hi))
        i1, i2 = range_of(tree[0], s, lo, hi)
        if i2 < i1:
            return ()
        return _candidates(tree[1:], syms[1:], i1, i2)
    else:
        return tree[0][lo:hi]


def _candidates_seq(tree: List[Sequence[int]], syms: Tuple[int, ...], inds: Iterable[int]) -> Iterable[int]:
    if syms:
        s = syms[0]
        if s is None:
            return _candidates_seq(tree[1:], syms[1:], inds)
        t0 = tree[0]
        return _candidates_seq(tree[1:], syms[1:], (i for i in inds if t0[i] == s))
    else:
        t0 = tree[0]
        return (t0[i] for i in inds)


def lo_hi_of(entries: Sequence[int], i2s: Sequence[int], x: int) -> Tuple[int, int]:
    # todo: use interpolation search
    i = bisect.bisect_left(entries, x)
    if entries[i] == x:
        if i == 0:
            return 0, i2s[i]
        else:
            return i2s[i - 1], i2s[i]
    else:
        return 1, 0


def range_of(xs: Sequence[T], y: T, lo: int, hi: int) -> Tuple[int, int]:
    i1 = bisect.bisect_left(xs, y, lo, hi)
    i2 = bisect.bisect_right(xs, y, i1, hi)
    return i1, i2


def count_candidates(wcs: Iterable[Tuple[int, int]]) -> Iterable[Tuple[int, int]]:
    d = {} # type: Dict[int, int]
    for w, c in wcs:
        if w in d:
            d[w] += c
        else:
            d[w] = c
    return d.items()


def optimize_query(ws: Tuple[Optional[T], ...]) -> Tuple[Optional[T], ...]:
    i = 0
    for w in ws:
        if w is None:
            i += 1
        else:
            break
    return ws[i:]


ones = itertools.repeat(1)


def zip_with_1(xs: Iterable[int]) -> Iterable[Tuple[int, int]]:
    return zip(xs, ones)


def encode(ws: Iterable[Optional[str]], sym_of_w: Mapping[str, int], not_found: int) -> Tuple[Optional[int], ...]:
    return tuple(_encode(w, sym_of_w, not_found) for w in ws)


def _encode(w: Optional[str], sym_of_w: Mapping[str, int], not_found: int) -> Optional[int]:
    if w is not None:
        return sym_of_w.get(w, not_found)


# -------- utilities


def txt_files_of(data_dir: str) -> List[str]:
    return [
        os.path.join(data_dir, f)
        for f
        in os.listdir(data_dir)
        if f.endswith('.txt')
    ]


def mtime_max_of(paths: Sequence[str]) -> int:
    return max(os.path.getmtime(path) for path in paths)


def read_and_split_all_txt(paths: Sequence[str]) -> List[str]:
    words = [] # type: List[str]
    for path in paths:
        with open(path) as fh:
            words.extend(w for w in fh.read().split())
    return words


def coding(xs: Sequence[K], code: Mapping[K, V]) -> List[V]:
    return [code[x] for x in xs]


def make_code(ws: Sequence[T]) -> Tuple[Dict[T, int],
                                        List[T]]:
    w_of_sym = sorted(set(ws))
    sym_of_w = dict.fromkeys(w_of_sym)
    for s, w in enumerate(w_of_sym):
        sym_of_w[w] = s
    return sym_of_w, w_of_sym


def make_type_code_of() -> Callable[[int], str]:
    type_codes = ('B', 'H', 'I', 'L')
    base = 2**8
    sizes = tuple(
        base**array.array(t, [0]).itemsize
        for t in type_codes
    )

    def type_code_of(n: int) -> str:
        assert n > -1
        for s, t in zip(sizes, type_codes):
            if n < s:
                return t
        return 'Q'
    return type_code_of


type_code_of = make_type_code_of()


def fuzzy_queries(ws: Tuple[str, ...]) -> Iterable[Tuple[Optional[str], ...]]:
    for q in itertools.product(
            *[(w, None)
              for w
              in reversed(ws)]
    ):
        yield tuple(reversed(q))


def each_cons(xs: Sequence[T], n: int) -> Sequence[Tuple[T, ...]]:
    assert n >= 1
    return _each_cons(xs, n)


def _each_cons(xs: Sequence[T], n: int) -> List[Tuple[T, ...]]:
    return [tuple(xs[i:i+n]) for i in range(len(xs) - (n - 1))]


if __name__ == '__main__':
    main(sys.argv)
