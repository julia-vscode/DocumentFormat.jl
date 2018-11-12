# TODO: strip extra newlines in merge_edits
# TODO: experiment with max_width

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
        elseif (t.kind == Tokens.TRIPLE_STRING || t.kind == Tokens.STRING) && t.startpos[1] != t.endpos[1]
            #= @info length(ranges), t =#
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
            return (l, offset - first(r) + 1, length(r))
        end
    end
    error("indexing range 1 - $(last(s.doc.ranges[end])), index used = $(offset)")
end
cursor_loc(s::State) = cursor_loc(s, s.offset)

function format(text::AbstractString; indent_width=4, max_width=90)
    d = Document(text, newline_ranges(text))
    s = State(indent_width, max_width, 0, 1, d)
    x = CSTParser.parse(text, true)
    e = pretty(x, s)::Edit
    if e.startline != 1
        e = merge_edits(Edit(1, 1, d.text[d.ranges[1]]), e, s)
    end
    if e.endline != length(d.ranges)
        e = merge_edits(e, Edit(length(d.ranges), length(d.ranges), d.text[d.ranges[end]]), s)
    end
    e.text
end

struct Edit
    startline::Int
    endline::Int
    text::AbstractString
end

# Returns an Edit, a prettified text representation of x
# along with the lines containing x in the original file.
#
# a
#
# comment 1
#
# comment 2
#
# b
#
function merge_edits(a::Edit, b::Edit, s::State; remove_extra_newlines=false)
    #= @info "Edit A", a, "Edit B", b =#

    if a.startline == b.startline || a.endline == b.endline
        text = a.text
        text *= b.text == "end" ? " " * b.text : b.text
        return Edit(a.startline, b.endline, text)
    end


    #= v = s.doc.text[s.doc.ranges[a.endline]] =#
    #= i = first(findfirst(x -> !isspace(x), v)) =#
    #= w = repeat(" ", max(i, s.indents * s.indent_width)) =#
    w = repeat(" ", s.indents * s.indent_width)

    # Due to the way CSTParser parses if statements, by default the condition
    # after the elseif keyword will be on a newline. For this reason we
    # handle merging elseif keyword expliticly.
    text = a.text == "" ? "\n" :
        endswith(a.text, "elseif") ? a.text * " " :
        a.text[end] != "\n" ?  a.text * "\n" * w :
        a.text

    for l in a.endline+1:b.startline-1
        v = s.doc.text[s.doc.ranges[l]]
        if v == "\n"
            text *= v * w
            continue
        end
        i = first(findfirst(x -> !isspace(x), v))
        if v[i] == '#'
            #= @info "COMMENT", v[first(ht):end] =#
            text *= v[i:end] * w
        else
            # This captures the possible additional indentation in a docstring
            i = max(min(i, s.indents-1 * s.indent_width), 1)
            #= @info "NON-COMMENT", i, v[i:end] =#
            text *= v[i:end] * w
        end
    end

    text *= b.text

    Edit(a.startline, b.endline, text)
end

Base.:*(a::Edit, b::T) where {T<:Union{AbstractString,AbstractChar}} = Edit(a.startline, a.endline, a.text * b)
Base.:*(a::T, b::Edit) where {T<:Union{AbstractString,AbstractChar}} = Edit(b.startline, b.endline, a * b.text)
merge_edits(a::Edit, b::T, s::State) where {T<:Union{AbstractString,AbstractChar}} = a * b
merge_edits(a::T, b::Edit, s::State) where {T<:Union{AbstractString,AbstractChar}} = a * b

function pretty(x::T, s::State) where {T<:Union{CSTParser.AbstractEXPR, Vector}}
    e = ""
    for a in x
        e = merge_edits(e, pretty(a, s), s)
    end
    e
end

function pretty(x::CSTParser.IDENTIFIER, s::State)
    loc = cursor_loc(s)
    s.offset += x.fullspan
    Edit(loc[1], loc[1], x.val)
end

function pretty(x::CSTParser.OPERATOR, s::State)
    loc = cursor_loc(s)
    s.offset += x.fullspan
    Edit(loc[1], loc[1], string(CSTParser.Expr(x)))
end

function pretty(x::CSTParser.KEYWORD, s::State)
    loc = cursor_loc(s)
    text = ""
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
    s.offset += x.fullspan
    Edit(loc[1], loc[1], text)
end

function pretty(x::CSTParser.PUNCTUATION, s::State; whitespace_after_comma=false)
    loc = cursor_loc(s)
    text = x.kind == Tokens.LPAREN ? "(" :
           x.kind == Tokens.LBRACE ? "{" :
           x.kind == Tokens.LSQUARE ? "[" :
           x.kind == Tokens.RPAREN ? ")" :
           x.kind == Tokens.RBRACE ? "}" :
           x.kind == Tokens.RSQUARE ? "]" :
           x.kind == Tokens.COMMA && whitespace_after_comma ? ", " :
           x.kind == Tokens.COMMA ? "," :
           x.kind == Tokens.SEMICOLON ? ";" :
           x.kind == Tokens.AT_SIGN ? "@" :
           x.kind == Tokens.DOT ? "." : ""
    s.offset += x.fullspan
    Edit(loc[1], loc[1], text)
end

function pretty(x::CSTParser.LITERAL, s::State; surround_with_quotes=true)
    loc = cursor_loc(s)
    text = x.kind == Tokens.TRIPLE_STRING && surround_with_quotes ? "\"\"\"" * escape_string(x.val, "\$") * "\"\"\"" :
           x.kind == Tokens.STRING && surround_with_quotes ? "\"" * escape_string(x.val, "\$") * "\"" :
           x.val
    s.offset += x.fullspan
    Edit(loc[1], loc[1], text)
end

function pretty(x::CSTParser.EXPR{CSTParser.StringH}, s::State)
    e = ""
    for a in x
        if a isa CSTParser.LITERAL
            e = merge_edits(e, pretty(a, s; surround_with_quotes=false), s)
        else
            e = merge_edits(e, pretty(a, s), s)
        end
    end
    Edit(e.startline, e.endline, "\"" * escape_string(e.text) * "\"")
end

function pretty(x::CSTParser.EXPR{CSTParser.MacroCall}, s::State)
    if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        pretty(x.args[1], s)

        o = s.offset
        loc1 = cursor_loc(s)
        s.offset += x.args[2].fullspan
        loc2 = cursor_loc(s, s.offset-1)
        #= @info "DOC POSITION START", loc1 =#
        #= @info "DOC POSITION END", loc2 =#

        tq = "\"\"\""
        w = repeat(" ", s.indent_width * s.indents)
        is_ts = startswith(s.doc.text[o:o+loc1[3]-loc1[2]], tq)
        quote_len = is_ts ? 3 : 1
        #= @info "STARTS WITH TRIPLE QUOTES", is_ts =#

        e = Edit(loc1[1], loc1[1], tq)

        if loc1[3] - loc1[2] > quote_len
            sidx = is_ts ? o + 3 : o + 1
            if loc1[1] == loc2[1]
                eidx = is_ts ? o+loc1[3]-loc1[2]-4 : o+loc1[3]-loc1[2]-2
                v = s.doc.text[sidx:eidx]
                #= @info "H1", v =#
            else
                #= eidx = is_ts ? o+loc1[3]-loc1[2]-1 : o+loc1[3]-loc1[2]-1 =#
                eidx = o+loc1[3]-loc1[2]-1
                v = s.doc.text[sidx:eidx]
                #= @info "H2", v =#
            end
            e = merge_edits(e, Edit(loc1[1], loc1[1], "\n" * w * v), s)
        end

        o = s.offset

        if loc1[1] == loc2[1]
            e = merge_edits(e, Edit(loc2[1]+1, loc2[1]+1, tq), s)
        elseif loc2[3] > quote_len + 1
            v = strip(is_ts ? s.doc.text[o-loc2[2]:o-5] : s.doc.text[o-loc2[2]:o-3])
            #= @info "H3", v =#
            if v  != ""
                e = merge_edits(e, Edit(loc2[1], loc2[1], v * "\n" * w * tq), s)
            else
                e = merge_edits(e, Edit(loc2[1], loc2[1], tq), s)
            end
        else
            e = merge_edits(e, Edit(loc2[1], loc2[1], tq), s)
        end

        return merge_edits(e, pretty(x.args[3], s), s)
    end

    e = ""
    for (i, a) in enumerate(x)
        # Macro calls can be whitespace sensitive
        if i == 1 || i == 2
            o = s.offset
            loc = cursor_loc(s)
            ei = pretty(a, s)
            if startswith(s.doc.text[o:o+loc[3]-loc[2]], ei.text * " ")
                e = merge_edits(e, ei, s) * " "
            else
                e = merge_edits(e, ei, s)
            end
        elseif i == length(x) - 1 && a isa CSTParser.PUNCTUATION && x.args[i+1] isa CSTParser.PUNCTUATION
            e = merge_edits(e, pretty(a, s), s)
        elseif a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA && i != length(x)
            e = merge_edits(e, pretty(a, s) * " ", s)
        else
            e = merge_edits(e, pretty(a, s), s)
        end
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.Block}, s::State; ignore_single_line=false)
    #= @info "INDENT START", s.indent_width * s.indents =#
    sl = !ignore_single_line ? cursor_loc(s)[1] == cursor_loc(s, s.offset+x.fullspan-1)[1] : false
    #= @info "SINGLE LINE BLOCK", x, length(x) =#
    e = ""
    for (i, a) in enumerate(x)
        if i == length(x) - 1 && a isa CSTParser.PUNCTUATION && x.args[i+1] isa CSTParser.PUNCTUATION
            e = merge_edits(e, pretty(a, s), s)
        elseif a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA && i != length(x)
            e = merge_edits(e, pretty(a, s) * " ", s)
        elseif sl
            if i == 1
                e = merge_edits(e, pretty(a, s), s)
            else
                e = merge_edits(e, "; " * pretty(a, s), s)
            end
        else
            e = merge_edits(e, pretty(a, s), s)
        end
    end
    #= @info "INDENT END", s.indent_width * s.indents =#
    e
end


# function foo end
# function foo body end
# function foo
#   body
# end
function pretty(x::CSTParser.EXPR{CSTParser.FunctionDef}, s::State)
    e = pretty(x.args[1], s)
    e = merge_edits(e, " " * pretty(x.args[2], s), s)
    if length(x) > 3
        sl = cursor_loc(s)[1] == cursor_loc(s, s.offset+x.args[3].fullspan+x.args[4].span-1)[1]
        sl && x.args[3].fullspan != 0 && (e *= " ")
        s.indents += 1
        e = merge_edits(e, pretty(x.args[3], s), s)
        s.indents -= 1
        e = merge_edits(e, pretty(x.args[4], s), s)
    else
        e = merge_edits(e, pretty(x.args[3], s), s)
    end
    e
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Macro,CSTParser.Struct}
    e = pretty(x.args[1], s)
    e = merge_edits(e, " " * pretty(x.args[2], s), s)
    sl = cursor_loc(s)[1] == cursor_loc(s, s.offset+x.args[3].fullspan+x.args[4].span-1)[1]
    sl && x.args[3].fullspan != 0 && (e *= " ")
    s.indents += 1
    e = merge_edits(e, pretty(x.args[3], s), s)
    s.indents -= 1
    merge_edits(e, pretty(x.args[4], s), s)
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.For,CSTParser.While}
    e = pretty(x.args[1], s)
    if x.args[2] isa CSTParser.EXPR{CSTParser.Block}
        e = merge_edits(e, " " * pretty(x.args[2], s; ignore_single_line=true), s)
    else
        e = merge_edits(e, " " * pretty(x.args[2], s), s)
    end
    sl = cursor_loc(s)[1] == cursor_loc(s, s.offset+x.args[3].fullspan+x.args[4].span-1)[1]
    sl && x.args[3].fullspan != 0 && (e *= " ")
    s.indents += 1
    e = merge_edits(e, pretty(x.args[3], s), s)
    s.indents -= 1
    merge_edits(e, pretty(x.args[4], s), s)
end


pretty(x::CSTParser.EXPR{CSTParser.Abstract}, s::State) = merge_edits(pretty(x.args[1:3], s), " " * pretty(x.args[4], s), s)
function pretty(x::CSTParser.EXPR{CSTParser.Mutable}, s::State)
    e = pretty(x.args[1], s)
    e = merge_edits(e, " " * pretty(x.args[2], s), s)
    e = merge_edits(e, " " * pretty(x.args[3], s), s)
    sl = cursor_loc(s)[1] == cursor_loc(s, s.offset+x.args[4].fullspan+x.args[5].span-1)[1]
    sl && x.args[4].fullspan != 0 && (e *= " ")
    s.indents += 1
    e = merge_edits(e, pretty(x.args[4], s), s)
    s.indents -= 1
    merge_edits(e, pretty(x.args[5], s), s)
end


function pretty(x::CSTParser.EXPR{CSTParser.Do}, s::State)
    e = pretty(x.args[1], s)
    e = merge_edits(e, " " * pretty(x.args[2], s), s)
    e = merge_edits(e, " " * pretty(x.args[3], s), s)
    if x.args[4] isa CSTParser.EXPR{CSTParser.Block}
        s.indents += 1
        e = merge_edits(e, pretty(x.args[4], s), s)
        s.indents -= 1
        e = merge_edits(e, pretty(x.args[5], s), s)
    else
        e = merge_edits(e, pretty(x.args[4], s), s)
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.Try}, s::State)
    e = pretty(x.args[1], s)
    s.indents += 1
    e = merge_edits(e, pretty(x.args[2], s), s)
    s.indents -= 1
    e = merge_edits(e, pretty(x.args[3], s), s)
    if x.args[4].fullspan != 0
        e = merge_edits(e, " " * pretty(x.args[4], s), s)
    end
    s.indents += 1
    e = merge_edits(e, pretty(x.args[5], s), s)
    s.indents -= 1
    e = merge_edits(e, pretty(x.args[6], s), s)
    if length(x.args) > 6
        s.indents += 1
        e = merge_edits(e, pretty(x.args[7], s), s)
        s.indents -= 1
        e = merge_edits(e, pretty(x.args[8], s), s)
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.ModuleH}, s::State)
    e = pretty(x.args[1], s) * " "
    merge_edits(e, pretty(x.args[2:end], s), s)
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Using,CSTParser.Import,CSTParser.Export}
    e = pretty(x.args[1], s) * " "
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
    #= @info x.op, x.op.kind, CSTParser.precedence(x.op) =#
    if CSTParser.precedence(x.op) in (8, 13, 14, 16) && x.op.kind != Tokens.ANON_FUNC
        e = merge_edits(e, pretty(x.op, s), s)
    elseif x.op.kind == Tokens.EX_OR
        e = merge_edits(e, " " * pretty(x.op, s), s)
    else
        e = merge_edits(e, " " * pretty(x.op, s) * " ", s)
    end
    merge_edits(e, pretty(x.arg2, s), s)
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
    if length(x.args[2]) == 0
        e *= " "
    else
        sl = cursor_loc(s)[1] == cursor_loc(s, s.offset+x.args[2].fullspan+x.args[3].span-1)[1]
        sl && x.args[2].fullspan != 0 && (e *= " ")
        s.indents += 1
        e = merge_edits(e, pretty(x.args[2], s), s)
        s.indents -= 1
    end
    merge_edits(e, pretty(x.args[3], s), s)
end

function pretty(x::CSTParser.EXPR{CSTParser.Quote}, s::State)
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.QUOTE
        e = pretty(x.args[1], s)
        if length(x.args[2]) == 0
            e *= " "
        else
            s.indents += 1
            e = merge_edits(e, pretty(x.args[2], s), s)
            s.indents -= 1
        end
        return merge_edits(e, pretty(x.args[3], s), s)
    end
    pretty(x.args, s)
end

function pretty(x::CSTParser.EXPR{CSTParser.Let}, s::State)
    e = ""
    if length(x.args) > 3
        e *= pretty(x.args[1], s) * " "
        if x.args[2] isa CSTParser.EXPR{CSTParser.Block}
            e = merge_edits(e, pretty(x.args[2], s; ignore_single_line=true), s)
        else
            e = merge_edits(e, pretty(x.args[2], s), s)
        end
        if length(x.args[3]) == 0
            e *= " "
        else
            s.indents += 1
            e = merge_edits(e, pretty(x.args[3], s), s)
            s.indents -= 1
        end
        e = merge_edits(e, pretty(x.args[4], s), s)
    else
        e = merge_edits(e, pretty(x.args[1], s), s)
        if length(x.args[2]) == 0
            e *= " "
        else
            s.indents += 1
            e = merge_edits(e, pretty(x.args[2], s), s)
            s.indents -= 1
        end
        e = merge_edits(e, pretty(x.args[3], s), s)
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.If}, s::State)
    e = pretty(x.args[1], s)
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.IF
        e = merge_edits(e, " " * pretty(x.args[2], s), s)
        sl = cursor_loc(s)[1] == cursor_loc(s, s.offset+x.args[3].fullspan-1)[1]
        sl && x.args[3].fullspan != 0 && (e *= " ")
        s.indents += 1
        e = merge_edits(e, pretty(x.args[3], s), s)
        s.indents -= 1
        e = merge_edits(e, pretty(x.args[4], s), s)
        if length(x.args) > 4
            s.indents += 1
            e = merge_edits(e, pretty(x.args[5].args[1], s), s)
            s.indents -= 1
            e = merge_edits(e, pretty(x.args[6], s), s)
        end
    else
        e = merge_edits(e, pretty(x.args[2], s), s)
        if length(x.args) > 2
            s.indents -= 1
            e = merge_edits(e, pretty(x.args[3], s), s)
            s.indents += 1
            e = merge_edits(e, pretty(x.args[4], s), s)
        end
    end
    e
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.Comparison,CSTParser.ChainOpCall,CSTParser.Kw,CSTParser.ChainOpCall}
    e = ""
    for (i, a) in enumerate(x)
        if a isa CSTParser.OPERATOR
            e = merge_edits(e, " " * pretty(a, s) * " ", s)
        elseif i == length(x) - 1 && a isa CSTParser.PUNCTUATION && x.args[i+1] isa CSTParser.PUNCTUATION
            e = merge_edits(e, pretty(a, s), s)
        elseif a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA && i != length(x)
            e = merge_edits(e, pretty(a, s) * " ", s)
        else
            e = merge_edits(e, pretty(a, s), s)
        end
    end
    e
end

function pretty(x::CSTParser.EXPR{CSTParser.Return}, s::State)
    e = pretty(x.args[1], s)
    if x.args[2].fullspan != 0
        e = merge_edits(e, " " * pretty(x.args[2:end], s), s)
    end
    e
end

function pretty(x::CSTParser.EXPR{T}, s::State) where T <: Union{CSTParser.TupleH,CSTParser.Call,CSTParser.Vect,CSTParser.Parameters}
    e = x isa CSTParser.EXPR{CSTParser.Parameters} ? "; " : ""
    for (i, a) in enumerate(x)
        if i == length(x) - 1 && a isa CSTParser.PUNCTUATION && x.args[i+1] isa CSTParser.PUNCTUATION
            e = merge_edits(e, pretty(a, s), s)
        elseif a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA && i != length(x)
            e = merge_edits(e, pretty(a, s) * " ", s)
        else
            e = merge_edits(e, pretty(a, s), s)
        end
    end
    e
end
