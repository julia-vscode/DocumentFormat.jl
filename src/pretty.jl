struct Document
    text::AbstractString
    ranges::Vector{UnitRange{Int}}
end

mutable struct State
    indent_width::Int
    max_width::Int
    indents::Int
    offset::Int
    doc::Document
end

function format(text::AbstractString; indent_width=4, max_width=120, indents=0)
    d = Document(text, newline_ranges(text))
    s = State(indent_width, max_width, indents, 1, d)
    x = CSTParser.parse(text, true)
    e = pretty(x, s)::Edit
    if e.startline != 1
        e = merge_edits(Edit(1, 1, d.text[d.ranges[1]]), e, s)
    end
    if e.endline != length(d.ranges)
        e = merge_edits(e, Edit(length(d.ranges), length(d.ranges), d.text[d.ranges[end]]), s)
    end
    e
end

struct Edit
    startline::Int
    endline::Int
    #= startbyte::Int =#
    text::AbstractString
    #= indents::Vector{Int} =#
end

# `pretty(f, x)` returns an Edit, which is essentially a prettified text repr of x
# along with the lines containing x in the original file.
#
# E1
#
# C1
#
# C2
#
# E2
#
# TODO: Either specify this is not commutative or
# add measures to make it so
function merge_edits(a::Edit, b::Edit, s::State)
    #= @info "Edit A", a, "Edit B", b =#
    if a.startline == b.startline || a.endline == b.endline
        return Edit(min(a.startline, b.startline), max(a.endline, b.endline), a.text * b.text)
    end

    text = a.text
    (text == "" || text[end] != "\n") && (text *= "\n")
    for l in a.endline+1:b.startline-1
        v = s.doc.text[s.doc.ranges[l]]
        ht = findfirst("#", v)
        if ht != nothing
            comment = v[first(ht):end]
            text *= s.indents > 0 ? repeat(" ", s.indent_width) * comment : comment
        else
            text *= v
        end
    end
    text *= b.text

    Edit(min(a.startline, b.startline), max(a.endline, b.endline), text)
end

Base.:*(a::Edit, b::T) where {T<:Union{AbstractString,AbstractChar}} = Edit(a.startline, a.endline, a.text * b)
Base.:*(a::T, b::Edit) where {T<:Union{AbstractString,AbstractChar}} = Edit(b.startline, b.endline, a * b.text)
merge_edits(a::Edit, b::T, s::State) where {T<:Union{AbstractString,AbstractChar}} = a * b
merge_edits(a::T, b::Edit, s::State) where {T<:Union{AbstractString,AbstractChar}} = a * b

function newline_ranges(text::String)
    ranges = UnitRange{Int}[]
    for t in CSTParser.Tokenize.tokenize(text)
        if t.kind == Tokens.WHITESPACE
            offset = t.startbyte
            for c in t.val
                if c == '\n'
                    s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
                    push!(ranges, s:offset+1)
                    # removes the newline at the end of every line
                    #= s = length(ranges) > 0 ? last(ranges[end]) + 1 : 0 =#
                    #= push!(ranges, s+1:offset) =#
                end
                offset += 1
            end
        elseif t.kind == Tokens.ENDMARKER
            s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
            push!(ranges, s:t.startbyte)
        elseif t.kind == Tokens.TRIPLE_STRING && t.startpos[1] != t.endpos[1]
            offset = t.startbyte
            nls = findall(x -> x == '\n', t.val)
            for nl in nls
                s = length(ranges) > 0 ? last(ranges[end]) + 1 : 1
                push!(ranges, s:offset+nl)
            end
        end
    end
    ranges
end

function cursor_loc(s::State, offset::Int)
    for (l, r) in enumerate(s.doc.ranges)
        if offset in r
            return (l, offset - first(r) + 1)
        end
    end
    error("indexing range 1 - $(last(s.doc.ranges[end])), index used = $(offset)")
end
cursor_loc(s::State) = cursor_loc(s, s.offset)

function isa_single_line(x::CSTParser.EXPR{T}, s::State) where {T}
    l1, _ = cursor_loc(s)
    offset = s.offset
    for a in x.args
        offset += a.fullspan
    end
    l2, _ = cursor_loc(s, offset)
    l1 == l2
end

function pretty(x::T, s::State) where {T<:Union{CSTParser.AbstractEXPR, Vector}}
    e = ""
    for a in x
        e = merge_edits(e, pretty(a, s), s)
    end
    e
end

function pretty(x::CSTParser.IDENTIFIER, s::State)
    l, _ = cursor_loc(s)
    s.offset += x.fullspan
    Edit(l, l, x.val)
end

function pretty(x::CSTParser.OPERATOR, s::State)
    l, _ = cursor_loc(s)
    s.offset += x.fullspan
    Edit(l, l, string(CSTParser.Expr(x)))
end

function pretty(x::CSTParser.KEYWORD, s::State)
    l, _ = cursor_loc(s)
    text = x.kind == Tokens.ABSTRACT ? "abstract" :
           x.kind == Tokens.BAREMODULE ? "baremodule" :
           x.kind == Tokens.BEGIN ? "begin" :
           x.kind == Tokens.BREAK ? "break" :
           x.kind == Tokens.CATCH ? "catch" :
           x.kind == Tokens.CONST ? "const" :
           x.kind == Tokens.CONTINUE ? "continue" :
           x.kind == Tokens.DO ? "do" :
           x.kind == Tokens.ELSE ? "else" :
           x.kind == Tokens.ELSEIF ? "elseif" :
           x.kind == Tokens.END ? "end" :
           x.kind == Tokens.EXPORT ? "export" :
           x.kind == Tokens.FINALLY ? "finally" :
           x.kind == Tokens.FOR ? "for" :
           x.kind == Tokens.FUNCTION ? "function" :
           x.kind == Tokens.GLOBAL ? "global" :
           x.kind == Tokens.IF ? "if" :
           x.kind == Tokens.IMPORT ? "import" :
           x.kind == Tokens.IMPORTALL ? "importall" :
           x.kind == Tokens.LET ? "let" :
           x.kind == Tokens.LOCAL ? "local" :
           x.kind == Tokens.MACRO ? "macro" :
           x.kind == Tokens.MODULE ? "module" :
           x.kind == Tokens.MUTABLE ? "mutable" :
           x.kind == Tokens.OUTER ? "outer" :
           x.kind == Tokens.PRIMITIVE ? "primitive" :
           x.kind == Tokens.QUOTE ? "quote" :
           x.kind == Tokens.RETURN ? "return" :
           x.kind == Tokens.STRUCT ? "struct" :
           x.kind == Tokens.TRY ? "try" :
           x.kind == Tokens.TYPE ? "type" :
           x.kind == Tokens.USING ? "using" :
           x.kind == Tokens.WHILE ? "while" : ""
    if text == "end" && indents == 0
        text *= "\n"
    elseif text == "end"
    elseif text == "do"
        text = " " * text * " "
    else
        text *= " "
    end
    s.offset += x.fullspan
    Edit(l, l, text)
end

function pretty(x::CSTParser.PUNCTUATION, s::State)
    l, _ = cursor_loc(s)
    text = x.kind == Tokens.LPAREN ? "(" :
           x.kind == Tokens.LBRACE ? "{" :
           x.kind == Tokens.LSQUARE ? "[" :
           x.kind == Tokens.RPAREN ? ")" :
           x.kind == Tokens.RBRACE ? "}" :
           x.kind == Tokens.RSQUARE ? "]" :
           x.kind == Tokens.COMMA ? "," :
           x.kind == Tokens.SEMICOLON ? ";" :
           x.kind == Tokens.AT_SIGN ? "@" :
           x.kind == Tokens.DOT ? "." : ""
    s.offset += x.fullspan
    Edit(l, l, text)
end

function pretty(x::CSTParser.LITERAL, s::State; quotes=true)
    l, _ = cursor_loc(s)
    text = x.kind == Tokens.STRING && quotes ? string("\"", x.val, "\"") : x.val
    s.offset += x.fullspan
    Edit(l, l, text)
end

function pretty(x::CSTParser.EXPR{CSTParser.StringH}, s::State; quotes=false)
    e = ""
    for a in x
        if a isa CSTParser.LITERAL
            e = merge_edits(e, pretty(a, s; quotes=quotes), s)
        else
            e = merge_edits(e, pretty(a, s), s)
        end
    end
    Edit(e.startline, e.endline, "\"" * escape_string(e.text) * "\"")
end

function pretty(x::CSTParser.EXPR{CSTParser.MacroCall}, s::State)
    if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        pretty(x.args[1], s)
        l1, _ = cursor_loc(s)
        #= @info "DOC START", l1 =#
        text = strip(pretty(x.args[2], s; quotes=false).text, ['\n'])
        text = join(escape_string.(split(text, '\n'), "\$"), "\n")
        #= s.offset += x.args[2].fullspan =#
        l2, _ = cursor_loc(s)
        #= @info "DOC END", l2 =#
        e = "\"\"\"\n" * Edit(l1, l2-1, text * '\n') * "\"\"\""
        #= e = "\"\"\"\n" * Edit(l1, l2-1, text * '\n') * "\"\"\"\n" =#
        #= e = merge_edits(Edit(l1, l1, "\"\"\"\n"), Edit(l2-1, l2-1, "\"\"\"\n"), s) =#
        return merge_edits(e, pretty(x.args[3], s), s)
    end
    merge_edits(pretty(x.args[1], s), " " * pretty(x.args[2:end], s), s)
end


# TODO: how to handle newlines?
function pretty(x::CSTParser.EXPR{CSTParser.Block}, s::State; indent=false)
    e = ""
    indent && (s.indents += 1)
    sl = isa_single_line(x, s)
    #= sl = false =#
    #= indent && (@info "INDENT START", s.indent_width * s.indents) =#
    for (i, a) in enumerate(x)
        if sl
            @info "SINGLE LINE BLOCK", a
            #= e = merge_edits(e, " " * pretty(a, s) * ";", s) =#
            if i > 1 && i < length(x.args)
                e = merge_edits(e, "; " * pretty(a, s), s)
            else
                e = merge_edits(e, " " * pretty(a, s), s)
            end
        elseif indent
            #= e = merge_edits(e, pretty(a, s) * "\n", s) =#
            ei = pretty(a, s)
            text = split(ei.text, '\n')
            text = join(repeat(" ", s.indent_width) .* text, "\n")
            #= e = merge_edits(e, Edit(ei.startline, ei.endline, text * "\n"), s) =#
            e = merge_edits(e, Edit(ei.startline, ei.endline, text), s)
        else
            #= e = merge_edits(e, pretty(a, s) * "\n", s) =#
            e = merge_edits(e, pretty(a, s), s)
        end
    end
    #= indent && (@info "INDENT END", s.indent_width * s.indents) =#
    indent && (s.indents -= 1)
    e
end


function pretty(x::CSTParser.EXPR{CSTParser.FunctionDef}, s::State)
    e = pretty(x.args[1:2], s)
    if length(x.args) > 3
        e = merge_edits(e, pretty(x.args[3], s; indent=true), s)
        e = merge_edits(e, pretty(x.args[4], s), s)
    else
        e = merge_edits(e, " " * pretty(x.args[3], s), s)
    end
    e
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Macro,CSTParser.For,CSTParser.While,CSTParser.Struct}
    e = pretty(x.args[1:2], s)
    #= e  length(x.args[3]) == 0 ? e * " " : merge_edits(e * "\n", pretty(x.args[3], s; indent=true), s) =#
    e = length(x.args[3]) == 0 ? e * " " : merge_edits(e, pretty(x.args[3], s; indent=true), s)
    merge_edits(e, pretty(x.args[4], s), s)
end


pretty(x::CSTParser.EXPR{CSTParser.Abstract}, s::State) = merge_edits(pretty(x.args[1:3], s), " " * pretty(x.args[4], s), s)
function pretty(x::CSTParser.EXPR{CSTParser.Mutable}, s::State)
    e = pretty(x.args[1:3], s)
    #= e = length(x.args[4]) == 0 ? e * " " : merge_edits(e, "\n" * pretty(x.args[4], s; indent=true), s) =#
    e = length(x.args[4]) == 0 ? e * " " : merge_edits(e, pretty(x.args[4], s; indent=true), s)
    merge_edits(e, pretty(x.args[5], s), s)
end


function pretty(x::CSTParser.EXPR{CSTParser.Do}, s::State)
    e = pretty(x.args[1:3], s)
    if x.args[4] isa CSTParser.EXPR{CSTParser.Block}
        #= e *= "\n" =#
        e = merge_edits(e, pretty(x.args[4], s; indent=true), s)
        e = merge_edits(e, pretty(x.args[5], s), s)
    else
        e = merge_edits(e, pretty(x.args[4], s), s)
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.Try}, s::State)
    e = pretty(x.args[1], s)
    e = merge_edits(e, pretty(x.args[2], s; indent=true), s)
    #= e = merge_edits(e, pretty(x.args[3:4], s) * "\n", s) =#
    e = merge_edits(e, pretty(x.args[3:4], s), s)
    e = merge_edits(e, pretty(x.args[5], s; indent=true), s)
    e = merge_edits(e, pretty(x.args[6], s), s)
    if length(x.args) > 6
        #= e *= "\n" =#
        e = merge_edits(e, pretty(x.args[7], s; indent=true), s)
        e = merge_edits(e, pretty(x.args[8], s), s)
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.ModuleH}, s::State)
    #= e = pretty(x.args[1:2], s) * "\n" =#
    e = pretty(x.args[1:2], s)
    e = merge_edits(e, pretty(x.args[3], s), s)
    merge_edits(e, pretty(x.args[4], s), s)
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Using,CSTParser.Import,CSTParser.Export}
    e = pretty(x.args[1], s)
    for a in x.args[2:end]
        if (a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA) || (a isa CSTParser.OPERATOR && a.kind == Tokens.COLON)
            e = merge_edits(e, pretty(a, s) * " ", s)
        else
            e = merge_edits(e, pretty(a, s), s)
        end
    end
	# TODO: check max width thingy
    e
end

function pretty(x::T, s::State) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    e = pretty(x.arg1, s)
    if CSTParser.precedence(x.op) in (8, 13, 14, 16)
        e = merge_edits(e, pretty(x.op, s), s)
    else
        e = merge_edits(e, " " * pretty(x.op, s) * " ", s)
    end
    # TODO: is checking if arg2 is a Block good enough?
    #= if x.arg2 isa CSTParser.EXPR{CSTParser.Block} && (CSTParser.defines_function(x) || CSTParser.defines_anon_function(x)) =#
    #=     e = merge_edits(e, pretty(x.arg2, s; single_line=true), s) =#
    #= else =#
    #=     e = merge_edits(e, pretty(x.arg2, s), s) =#
    #= end =#
    e = merge_edits(e, pretty(x.arg2, s), s)
    e
end

function pretty(x::CSTParser.ConditionalOpCall, s::State)
    e = pretty(x.cond, s)
    e = merge_edits(e, " " * pretty(x.op1, s) * " ", s)
    e = merge_edits(e, pretty(x.arg1, s), s)
    e = merge_edits(e, " " * pretty(x.op2, s) * " ", s)
    merge_edits(e, pretty(x.arg2, s), s)
end

function pretty(x::CSTParser.WhereOpCall, s::State)
    e = pretty(x.arg1, s)
    e = merge_edits(e, " " * pretty(x.op, s) * " ", s)
    merge_edits(e, pretty(x.args, s), s)
end

function pretty(x::CSTParser.EXPR{CSTParser.Begin}, s::State)
    e = pretty(x.args[1], s)
    #= e = merge_edits(e, pretty(x.args[2], s; indent=true), s) =#
    #= e = length(x.args[2]) == 0 ? e * " " : merge_edits(e * "\n", pretty(x.args[2], s; indent=true), s) =#
    e = length(x.args[2]) == 0 ? e * " " : merge_edits(e, pretty(x.args[2], s; indent=true), s)
    merge_edits(e, pretty(x.args[3], s), s)
end

function pretty(x::CSTParser.EXPR{CSTParser.Quote}, s::State)
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.QUOTE
        e = pretty(x.args[1], s)
        #= e = length(x.args[2]) == 0 ? e * " " : merge_edits(e * "\n", pretty(x.args[2], s; indent=true), s) =#
        e = length(x.args[2]) == 0 ? e * " " : merge_edits(e, pretty(x.args[2], s; indent=true), s)
        return merge_edits(e, pretty(x.args[3], s), s)
    end
    pretty(x.args, s)
end

function pretty(x::CSTParser.EXPR{CSTParser.Let}, s::State)
    e = ""
    if length(x.args) > 3
        e = merge_edits(e, pretty(x.args[1:2], s), s)
        #= e = merge_edits(e, length(x.args[3]) == 0 ? " " : "\n" * pretty(x.args[3], s; indent=true), s) =#
        e = merge_edits(e, length(x.args[3]) == 0 ? " " : pretty(x.args[3], s; indent=true), s)
        e = merge_edits(e, pretty(x.args[4], s), s)
    else
        e = merge_edits(e, pretty(x.args[1], s), s)
        e = merge_edits(e, length(x.args[2]) == 0 ? " " : pretty(x.args[2], s; indent=true), s)
        e = merge_edits(e, pretty(x.args[3], s), s)
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.If}, s::State)
    #= e = pretty(x.args[1:2], s) * "\n" =#
    e = pretty(x.args[1], s)
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.IF
        e = merge_edits(e, pretty(x.args[2], s), s)
        e = merge_edits(e, pretty(x.args[3], s; indent=true), s)
        e = merge_edits(e, pretty(x.args[4], s), s)
        if length(x.args) > 4
            #= e *= "\n" =#
            #= e = merge_edits(e, pretty(x.args[5], s; indent=true), s) =#
            e = merge_edits(e, pretty(x.args[5], s), s)
            e = merge_edits(e, pretty(x.args[6], s), s)
        end
    else
        #= e = merge_edits(e, pretty(x.args[2:end], s), s) =#
        e = merge_edits(e, pretty(x.args[2], s), s)
        if length(x.args) > 2
            e = merge_edits(e, pretty(x.args[3], s), s)
            e = merge_edits(e, pretty(x.args[4], s), s)
        end
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.Comparison}, s::State)
    e = ""
    for a in x
        if a isa CSTParser.OPERATOR
            e = merge_edits(e, " " * pretty(a, s) * " ", s)
        else
            e = merge_edits(e, pretty(a, s), s)
        end
    end
    e
end

pretty(x::CSTParser.EXPR{CSTParser.Parameters}, s::State) = "; " * pretty(x.args, s)
function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.TupleH,CSTParser.ChainOpCall,CSTParser.Call,CSTParser.Vect}
    e = ""
    for a in x
        if a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA
            e = merge_edits(e, pretty(a, s) * " ", s)
        else
            e = merge_edits(e, pretty(a, s), s)
        end
    end
    e
end
