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
end


let
    dir = joinpath(dirname(@__FILE__), "t2")
    data = load(dir)
    @test data == include(joinpath(dir, "data.jl"))
end
