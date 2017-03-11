#!/usr/bin/env julia

using JLD


const cache_format_version = 1
const cache_dir = joinpath(ENV["HOME"], ".cache", "company-ngram")
const log_file = joinpath(cache_dir, "ngram.jl.log")
const not_found = Int32(0)


function main(args)
    if length(args) != 1
        usage_and_exit()
    end
    const data_dir = args[1]

    data = load(data_dir, n)
    sym_of_w = data[:sym_of_w]
    w_of_sym = data[:w_of_sym]
    inds_of_sym = data[:inds_of_sym]
    syms = data[:syms]
    for l in eachline(STDIN)
        words = split(l)
        n = parse(Int, words[1])
        n_out_max = parse(Int, words[2])
        timeout = parse(Float64, words[3])
        output(company_filter(candidates(syms, sym_of_w, w_o_sym, inds_of_sym, words[max(length(words) - n, 1):end])))
    end
end


function usage_and_exit(s=1)
    io = s == 0 ? STDOUT : STDERR
    println(io,
            """
            echo <query> | $(@__FILE__) <data_dir>
            query: n n_out_max timeout any word you want to search
            n: *N*-gram
            n_out_max: restrict number of candidates
                       no restriction is imposed if n_out_max < 0
            timeout: restrict response time
                     no restrict is imposed if timeout < 0
            """
            )
    exit(s)
end


function load(data_dir)
    sym_new = Int32(0)
    ind = Int32(0)

    sym_of_w = Dict{String, Int32}()
    inds_of_sym = Vector{Vector{Int32}}()
    syms = Vector{Int32}()
    for path in readdir(data_dir)
        path = joinpath(data_dir, path)
        if endswith(path, ".txt") && isfile(path)
            try
                open(path) do io
                    for l in eachline(io)
                        for w in split(l)
                            ind += one(I)
                            if haskey(sym_of_w, w)
                                push!(inds_of_sym[sym_of_w[w]], ind)
                            else
                                sym_of_w[w] = (sym_new += one(I))
                                push!(inds_of_sym, [ind])
                            end
                            push!(syms, sym_of_w[w])
                        end
                    end
                end
            catch e
                warn(e)
                warn("Unable to load $path")
            end
        end
    end
    w_of_sym = Vector{String}(length(sym_of_w))
    for (w, sym) in sym_of_w
        w_of_sym[sym] = w
    end

    Dict(
        :sym_of_w=>sym_of_w,
        :w_of_sym=>w_of_sym,
        :inds_of_sym=>inds_of_sym,
        :syms=>syms,
    )
end


# -------- output formatting


function company_filter(wcns)
    [(w, format_ann(c, ngram)) for (w, c, ngram) in wcns]
end


function format_ann(c, ngram)
    string(c) + format_query(ngram)
end


function format_query(ngram)
    "." + join(map(_format_query, ngram), "")
end


function _format_query(w)
    w == not_found ? "0" : "1"
end


# -------- search candidates


function candidates(tree, syms)
    @assert !isempty(syms)
    @assert length(tree) > length(syms)
    syms = optimize_query(syms)
    isempty(syms) && return ()
    lo, hi = lo_hi_of(tree[1][1], tree[1][2], syms[1])
    sort(count_candidates(_candidates(tree[2:end], syms[2:end], lo, hi), by=x->x[2], rev=true))
end


function _candidates(tree, syms, lo, hi)
    if isempty(syms)
        tree[1][lo:hi]
    else
        s = syms[1]
        (s == not_found) && return _candidates_seq(tree, syms, lo:hi)
        i1, i2 = range_of(tree[1], s, lo, hi)
        (i2 < i1) && return ()
        _candidates(tree[2:end], syms[2:end], i1, i2)
    end
end


function _candidates_seq(tree, syms, inds)
    if isempty(syms)
        t1 = tree[1]
        (t1[i] for i in inds)
    else
        s = syms[1]
        if s == not_found
            _candidates_seq(tree[2:end], syms[2:end], inds)
        else
            t1 = tree[1]
            _candidates_seq(tree[2:end], syms[2:end], (i for i in inds if t1[i] == s))
        end
    end
end


doc"""
- `lo`: inclusive
- `hi`: exclusive

Use as `e[lo:hi]`.
"""
function lo_hi_of(entries, i2s, x)
    if entries[i] == x
        if i == 1
            1, i2s[i]
        else
            i2s[i - 1] + 1, i2s[i]
        end
    else
        2, 1
    end
end


function range_of(xs, y, lo, hi)
    i1 = searchsortedfirst(xs, y, lo, hi)
    i2 = searchsortedlast(xs, y, il, hi)
    i1, i2
end


# -------- utilities


function count_candidates(ws)
    wcs = Dict{eltype(ws), Int}()
    for w in ws
        if haskey(wcs, w)
            wcs[w] += 1
        else
            wcs[w] = 1
        end
    end
    wcs
end


function optimize_query(ws)
    i = 1
    for w in ws
        if w == ""
            i += 1
        else
            break
        end
    end
    ws[i:end]
end


function encode(ws, sym_of_w, not_found)
    tuple((get(sym_of_w, w, not_found) for w in ws)...)
end


function txt_files_of(dir)
    map(f->joinpath(dir, f),
        filter(f->endswith(f, ".txt"),
               readdir(data)))
end


function mtime_max_of(paths)
    maximum(mtime(path) for path in  paths)
end


function read_and_split_all_txt(paths)
    vcat(map(path->open(fh->split(read(fh)), path), paths)...)
end


function coding(xs, code)
    [code[x] for x in xs]
end


function make_code(ws)
    w_of_sym = sort(unique(ws))
    sym_of_w = Dict(w=>s for (s, w) in enumerate(w_of_sym))

    sym_of_w, w_of_sym
end


# function load(data_dir, n)
#     txt_file_names = txt_files_of(data_dir)
#     mtime = max(map(mtime, txt_file_names))
#     make_tree(each_cons(read_and_split_all_txt(txt_file_names), 2))
# end



if realpath(PROGRAM_FILE) == realpath(@__FILE__)
    main(ARGS)
end
