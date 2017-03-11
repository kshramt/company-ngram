#!/usr/bin/env julia

using JLD


const cache_format_version = 1
const cache_dir = joinpath(ENV["HOME"], ".cache", "company-ngram")
const log_file = joinpath(cache_dir, "ngram.jl.log")
const n_inds_split = 32
const not_found = Int32(0)


abstract AbstractCons

immutable Nil <: AbstractCons
end

const nil = Nil()

immutable Cons{A, D} <: AbstractCons
    a::A
    d::D
end

function car(c::Cons)
    c.a
end

function cdr(c::Cons)
    c.d
end

function ncons(n, a, c)
    for _ in 1:n
        c = Cons(a, c)
    end
    c
end

function Base.start(c::AbstractCons)
    c
end

function Base.done(::AbstractCons, s)
    s == nil
end

function Base.next(::AbstractCons, s)
    car(s), cdr(s)
end


function main(args)
    if length(args) != 1
        usage_and_exit()
    end
    const data_dir = args[1]

    data = load(data_dir)
    sym_of_w = data[:sym_of_w]
    w_of_sym = data[:w_of_sym]
    inds_of_sym = data[:inds_of_sym]
    syms = data[:syms]

    inds_cache = Dict{AbstractCons, Vector{eltype(syms)}}()
    for l in eachline(STDIN)
        words = split(l)
        isempty(words) && continue
        if words[1] == "command"
            if length(words) > 2
                if words[2] == "save_cache"
                end
            end
        elseif length(words) > 3
            n = parse(Int32, words[1])
            n_out_max = parse(Int32, words[2])
            timeout = parse(Float64, words[3])
            candidates(
                make_output(STDOUT, syms, w_of_sym, Set{eltype(syms)}())...,
                [get(sym_of_w, words[i], not_found) for i in max(length(words) - n, 4):length(words)],
                syms,
                inds_of_sym,
                inds_cache,
            )
        end
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
    for path in sort(readdir(data_dir))
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

function make_output(io, syms, w_of_sym, seen)
    function output(prefix, inds)
        for (sym, cnt) in sort_candidates(count_candidates(inds, syms))
            if !(sym in seen)
                println(io, w_of_sym[sym], "\t", cnt, ".", format_prefix(prefix))
                push!(seen, sym)
            end
        end
    end

    function end_of_output()
        print(io, "\n\n")
        flush(io)
    end

    output, end_of_output
end


function format_prefix(prefix)
    join((x == not_found ? "0" : "1" for x in prefix), "")
end


function sort_candidates(count_of_sym)
    sort([kv for kv in count_of_sym], by=kv->kv[2], rev=true)
end


function count_candidates{I}(inds::Vector{I}, syms::Vector{I})
    count_of_sym = Dict{I, I}()
    for ind in inds
        if (-1 < ind < length(syms))
            sym = syms[ind + 1]
            if haskey(count_of_sym, sym)
                count_of_sym[sym] += 1
            else
                count_of_sym[sym] = 1
            end
        end
    end
    count_of_sym
end


# -------- search candidates


function candidates{I}(
    output,
    end_of_output,
    full_prefix,
    syms,
    inds_of_sym::Vector{Vector{I}},
    inds_cache::Dict{AbstractCons, Vector{I}},
)
    @assert length(full_prefix) > 0

    for shift in I(0):I((length(full_prefix) - 1))
        sym = full_prefix[end - shift]
        prefix = Cons(sym, ncons(shift, not_found, nil))
        if 0 < sym <= length(syms)
            inds = if shift == 0
                inds_of_sym[sym]
            else
                inds_of_sym[sym] + shift
            end
            _candidates(
                output,
                full_prefix,
                syms,
                inds_of_sym,
                inds_cache,
                shift,
                prefix,
                inds,
            )
         end
    end
    end_of_output()
end

function _candidates{I}(
    output,
    full_prefix,
    syms,
    inds_of_sym::Vector{Vector{I}},
    inds_cache::Dict{AbstractCons, Vector{I}},
    base_shift,
    base_prefix,
    base_inds,
)
    isempty(base_inds) && return
    for shift in (base_shift + 1):(length(full_prefix) - 1)
        sym = full_prefix[end - shift]
        prefix = Cons(sym, ncons(shift - base_shift - 1, not_found, base_prefix))
        if 0 < sym <= length(syms)
            inds = if haskey(inds_cache, prefix)
                inds_cache[prefix]
            else
                inds_cache[prefix] = [ind for ind in base_inds if get(syms, ind - shift, not_found) == sym]
            end
            _candidates(
                output,
                full_prefix,
                syms,
                inds_of_sym,
                inds_cache,
                shift,
                prefix,
                inds,
            )
        end
    end
    output(base_prefix, base_inds)
end


function txt_files_of(dir)
    map(f->joinpath(dir, f),
        filter(f->endswith(f, ".txt"),
               readdir(data)))
end


function mtime_max_of(paths)
    maximum(mtime(path) for path in  paths)
end



if abspath(PROGRAM_FILE) == abspath(@__FILE__)
    main(ARGS)
end
