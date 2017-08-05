using DocumentFormat: format, formatpkg
using Base.Test

@testset "All" begin
@testset "basic" begin
    @test format("a") == "a"
end
@testset "tuples" begin 
    @test format("a,b") == "a, b"
    @test format("a ,b") == "a, b"
    @test format("(a,b)") == "(a, b)"
    @test format("(a ,b)") == "(a, b)"
    @test format("( a, b)") == "(a, b)"
    @test format("(a, b )") == "(a, b)"
    @test format("(a, b ,)") == "(a, b,)"
end
@testset "curly" begin 
    @test format("X{a,b}") == "X{a,b}"
    @test format("X{ a,b}") == "X{a,b}"
    @test format("X{a ,b}") == "X{a,b}"
    @test format("X{a, b}") == "X{a,b}"
    @test format("X{a,b }") == "X{a,b}"
    @test format("X{a,b }") == "X{a,b}"
end
@testset "unary ops" begin
    @test format("! x") == "!x"
end
@testset "unary ops" begin
    @test format("a?b:c") == "a ? b : c"
    @test format("a ?b:c") == "a ? b : c"
    @test format("a? b:c") == "a ? b : c"
    @test format("a?b :c") == "a ? b : c"
    @test format("a?b: c") == "a ? b : c"
end
@testset "binary ops" begin
    @test format("a+b*c") == "a + b * c"
    @test format("a +b*c") == "a + b * c"
    @test format("a+ b*c") == "a + b * c"
    @test format("a+b *c") == "a + b * c"
    @test format("a+b* c") == "a + b * c"
    @test format("a+b*c ") == "a + b * c"
    @test format("a:b") == "a:b"
    @test format("a : b") == "a:b"
    @test format("a: b") == "a:b"
    @test format("a :b") == "a:b"
    @test format("a:b:c") == "a:b:c"
    @test format("a :b:c") == "a:b:c"
    @test format("a: b:c") == "a:b:c"
    @test format("a:b :c") == "a:b:c"
    @test format("a:b: c") == "a:b:c"
    @test format("a:b:c ") == "a:b:c"
    @test format("a::b:: c") == "a::b::c"
    @test format("a :: b::c") == "a::b::c"
end

@testset "func call" begin
    @test format("func(a, b, c)") == "func(a, b, c)"
    @test format("func(a,b,c)") == "func(a, b, c)"
    @test format("func( a,b,c    )") == "func(a, b, c)"
    @test format("func(a, b, c)  ") == "func(a, b, c)"
    @test format("func(a, b; c)") == "func(a, b; c)"
    @test format("func(  a, b; c)") == "func(a, b; c)"
    @test format("func(a  ,b; c)") == "func(a, b; c)"
end

@testset "indents" begin
@testset "begin" begin
        str = """
        begin
            arg
        end"""
        @test format("""
                    begin
                    arg
                    end""") == str
        @test format("""
                    begin
                     arg
                    end""") == str
        @test format("""
                    begin
                      arg
                    end""") == str
        @test format("""
                    begin
                          arg
                    end""") == str
        str = """
        begin
            begin
                arg
            end
        end"""
        @test format("""
                    begin
                    begin
                    arg
                    end
                    end""") == str
                    @test format("""
                    begin
                                begin
                    arg
                    end
                    end""") == str
        @test format("""
                    begin
                                begin
                    arg
                            end
                    end""") == str
    end
end
end

@testset "formatpkg DocumentFormat" begin
    mktempdir() do path
        cp(joinpath(@__DIR__, ".."), joinpath(path, "DocumentFormat"))
        formatpkg(joinpath(path, "DocumentFormat"); force = true)
    end
end

include("test_formatconfig.jl")
