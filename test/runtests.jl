#!/usr/bin/env julia

using Base.Test: @test


include("../ngram.jl")


function pp(x)
    _pp("", x)
    println()
end

function _pp(indent, x::Vector)
    println(indent, "[")
    ii = indent*"    "
    for v in x
        _pp(ii, v)
        println(",")
    end
    print(indent, "]")
end

function _pp(indent, x::Tuple)
    println(indent, "(")
    ii = indent*"    "
    for v in x
        _pp(ii, v)
        println(",")
    end
    print(indent, ")")
end

function _pp(indent, x::Dict)
    println(indent, "Dict(")
    ii = indent*"    "
    for kv in x
        _pp(ii, kv)
        println(",")
    end
    print(indent, ")")
end

function _pp(indent, x::Pair)
    k, v = x
    _pp(indent, k)
    println(" =>")
    _pp(indent, v)
end

function _pp(indent, x::Int32)
    print(indent, "Int32(", x, ")")
end

function _pp(indent, x)
    print(indent, repr(x))
end


## test begin


let
    dir = joinpath(dirname(@__FILE__), "t1")
    data = load(dir)
    @test data == include(joinpath(dir, "data.jl"))

    let
        int = eltype(data[:syms])
        seen = Set{int}()
        inds_cache = Dict{AbstractCons, Vector{int}}()
        io = IOBuffer(UInt8[], true, true)
        candidates(
            make_output(io, data[:syms], data[:w_of_sym], seen)...,
            [Int32(1), Int32(2)],
            data[:syms],
            data[:inds_of_sym],
            inds_cache,
        )
        flush(io)
        out = readstring(seekstart(io))
        @test out == "c\t1.11\n\n\n"
    end

    let
        int = eltype(data[:syms])
        inds_cache = Dict{AbstractCons, Vector{int}}()
        output_args = []
        candidates(
            ()->nothing,
            (int(1), int(2)),
            data[:syms],
            data[:inds_of_sym],
            inds_cache,
        ) do prefix, inds
            push!(output_args, (prefix, inds))
        end
        @test output_args == [
            (Cons(int(1), Cons(int(2), nil)), int[2, 5]),
            (Cons(int(2), nil), int[2, 5]),
            (Cons(int(1), Cons(int(0), nil)), int[2, 5]),
        ]
        @test inds_cache == Dict(
            Cons(int(1), Cons(int(2), nil)) => int[2, 5],
        )
    end
end


let
    dir = joinpath(dirname(@__FILE__), "t2")
    data = load(dir)
    @test data == include(joinpath(dir, "data.jl"))

    let
        int = eltype(data[:syms])
        inds_cache = Dict{AbstractCons, Vector{int}}()
        output_args = []
        prefix = (data[:sym_of_w]["pen"], data[:sym_of_w]["pineapple"], data[:sym_of_w]["apple"])
        candidates(
            ()->nothing,
            prefix,
            data[:syms],
            data[:inds_of_sym],
            inds_cache,
        ) do prefix, inds
            push!(output_args, (prefix, inds))
        end
        @test output_args == include(joinpath(dir, "output_args_1.jl"))
        @test inds_cache == include(joinpath(dir, "inds_cache_1.jl"))
    end
end
