module DocumentFormat
using CSTParser
import Tokenize.Tokens
import CSTParser: EXPR, OPERATOR, IDENTIFIER, PUNCTUATION
import CSTParser: UnaryOpCall, UnarySyntaxOpCall, BinaryOpCall, BinarySyntaxOpCall
import CSTParser: trailing_ws_length, precedence

export format
const cst = CSTParser

include("options.jl")
include("formatconfig.jl")

struct TextEdit
    range::UnitRange{Int}
    text::String
end
Deletion(range::UnitRange{Int}) = TextEdit(range, "")

mutable struct Diagnostic
    message::String
    edits::Vector{TextEdit} 
end

mutable struct FormatState
    content::String
    offset::Int
    diagnostics::Vector{Diagnostic}
    line_ranges::Vector{UnitRange{Int}}
    indent::Int
    config::FormatConfig
end
FormatState(str) = FormatState(str, 0, [], get_line_ranges(str), 0, FormatConfig())



include("operators.jl")
include("indents.jl")
include("utils.jl")

function format(str::String)
    x = CSTParser.parse(str, true)
    F = FormatState(str)
    format(x, F)
    F.config.StripLineEnds && strip_empty_line_ends(F)
    F.config.NewLineEOF && end_file_newline(F)
    apply(str, F)
end

function format(x::EXPR{T}, F::FormatState) where T
    if T <: IndentEXPR
        indent(F)
    end
    for a in x.args
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    if T <: IndentEXPR
        deindent(F)
    end
end

function format(x::EXPR{cst.TupleH}, F::FormatState)
    nargs = length(x.args)
    hasbrackets = first(x.args) isa EXPR{PUNCTUATION{Tokens.LPAREN}}
    for (i, a) in enumerate(x.args)
        offset = F.offset
        if a isa EXPR{PUNCTUATION{Tokens.COMMA}} && !(hasbrackets && i == nargs  - 1)
            i < nargs && trailing_ws(a, F)
        else
            i < nargs && no_trailing_ws(a, F)
            format(a, F)
        end
        F.offset = offset + a.fullspan
    end
end

function format(x::EXPR{cst.Curly}, F::FormatState)
    nargs = length(x.args)
    for (i, a) in enumerate(x.args)
        offset = F.offset
        if a isa EXPR{PUNCTUATION{Tokens.LPAREN}} || (i == nargs - 1 && !(x.args[i] isa EXPR{PUNCTUATION{Tokens.COMMA}}))
            no_trailing_ws(a, F)
        elseif i < nargs 
            no_trailing_ws(a, F)
            format(a, F)
        end
        F.offset = offset + a.fullspan
    end
end

function format(x::EXPR{cst.Call}, F::FormatState)
    nargs = length(x.args)
    for (i, a) in enumerate(x.args)
        offset = F.offset
        if a isa EXPR{PUNCTUATION{Tokens.COMMA}} 
            trailing_ws(a, F)
        else
            i < nargs && !(x.args[i+1] isa EXPR{cst.Parameters}) && no_trailing_ws(a, F)
            format(a, F)
        end
        F.offset = offset + a.fullspan
    end
end

end
