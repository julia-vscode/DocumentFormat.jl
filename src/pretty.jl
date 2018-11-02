INDENT_WIDTH = 4
NL = '\n'
ws(n::Int) = repeat(' ', n)

pretty(x) = pretty(x, 0)
pretty(x::T, width) where {T<:Union{CSTParser.AbstractEXPR, Vector}} = mapreduce(y -> pretty(y, width), *, x; init="")
pretty(x::T, width) where {T <: Union{CSTParser.IDENTIFIER,CSTParser.LITERAL}} = x.val
pretty(x::CSTParser.OPERATOR, width) = string(CSTParser.Expr(x))
pretty(x::CSTParser.EXPR{CSTParser.MacroName}, width) = string(CSTParser.Expr(x))
#= pretty(x::CSTParser.KEYWORD, width) = x.kind |> string |> lowercase =#

function pretty(x::CSTParser.KEYWORD, width)
    s = x.kind |> string |> lowercase
    x.kind == Tokens.END && return s * NL
    x.kind == Tokens.DO && return ws(1) * s * ws(1)
    #= x.kind == Tokens.DO && (return ws(1) * s * ws(1)) =#
    return s * ws(1)
end

function pretty(x::CSTParser.PUNCTUATION, width)
    x.kind == Tokens.LPAREN && return "("
    x.kind == Tokens.LBRACE && return "{"
    x.kind == Tokens.LSQUARE && return "["
    x.kind == Tokens.RPAREN && return ")"
    x.kind == Tokens.RBRACE && return "}"
    x.kind == Tokens.RSQUARE && return "]"
    x.kind == Tokens.COMMA && return ","
    x.kind == Tokens.SEMICOLON && return ";"
    x.kind == Tokens.AT_SIGN && return "@"
    x.kind == Tokens.DOT && return "."
    return ""
end

pretty(x::CSTParser.EXPR{CSTParser.Parameters}, width) = "; " * pretty(x.args, width)

pretty(x::CSTParser.LITERAL, width; in_doc=false) = x.kind == Tokens.STRING && !in_doc ? string("\"", x.val, "\"") : x.val
function pretty(x::CSTParser.EXPR{CSTParser.StringH}, width; in_doc=false)
    s = in_doc ? "" : "\""
    for a in x
        if a isa CSTParser.LITERAL
            s *= pretty(a, width; in_doc=in_doc)
        else
            s *= pretty(a, width)
        end
    end
    s *= in_doc ? "" : "\""
end

function pretty(x::CSTParser.EXPR{CSTParser.MacroCall}, width)
    if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        s = "\"\"\"\n"
        s *= strip(pretty(x.args[2], width; in_doc=true), ['\n']) * NL
        s *= "\"\"\"\n"
        return s * pretty(x.args[3], width)
    end
    pretty(x.args[1], width) * ws(1) * pretty(x.args[2:end], width)
end

function pretty(x::CSTParser.EXPR{CSTParser.Block}, width; indent=false)
    s = ""
    for a in x
        if indent
            ss = split(pretty(a, width+INDENT_WIDTH), "\n")
            #= ss[end] == "" && (ss = s[1:end-1]) =#
            #= @info length(ss), ss =#
            s *= join(ws(INDENT_WIDTH) .* ss, "\n") * NL
        else
            s *= pretty(a, width)
        end
    end
    s
end

function pretty(x::CSTParser.EXPR{CSTParser.FunctionDef}, width)
    s = pretty(x.args[1], width) * pretty(x.args[2], width)
    if x.args[3] isa CSTParser.EXPR{CSTParser.Block}
        s *= NL
        s *= pretty(x.args[3], width; indent=true)
        s *= pretty(x.args[4], width)
    else
        s *= ws(1) * pretty(x.args[3], width)
    end
    s
end

function pretty(x::CSTParser.EXPR{T}, width) where T <: Union{CSTParser.Macro,CSTParser.For,CSTParser.While,CSTParser.Struct}
    s = pretty(x.args[1], width) * pretty(x.args[2], width)
    s *= length(x.args[3]) == 0 ? " " : NL * pretty(x.args[3], width; indent=true)
    s * pretty(x.args[4], width)
end


function pretty(x::CSTParser.EXPR{CSTParser.Mutable}, width)
    s = pretty(x.args[1:3], width)
    s *= length(x.args[4]) == 0 ? " " : NL * pretty(x.args[4], width; indent=true)
    s * pretty(x.args[5], width)
end

function pretty(x::CSTParser.EXPR{CSTParser.Do}, width)
    s = pretty(x.args[1:3], width)
    if x.args[4] isa CSTParser.EXPR{CSTParser.Block}
        s *= pretty(x.args[4], width; indent=true)
        s *= pretty(x.args[5], width)
    else
        s *= pretty(x.args[4], width)
    end
    s
end

function pretty(x::CSTParser.EXPR{CSTParser.Try}, width)
    s = pretty(x.args[1], width)
    s *= pretty(x.args[2], width; indent=true)
    s *= pretty(x.args[3], width) * pretty(x.args[4], width) * NL
    s *= pretty(x.args[5], width; indent=true)
    s *= pretty(x.args[6])
    if length(x.args) > 6
        s *= NL
        s *= pretty(x.args[7], width; indent=true)
        s *= pretty(x.args[8])
    end
    s
end

function pretty(x::CSTParser.EXPR{CSTParser.ModuleH}, width)
    s = pretty(x.args[1], width) * pretty(x.args[2], width) * NL
    s *= pretty(x.args[3], width)
    s * pretty(x.args[4])
end

function pretty(x::CSTParser.EXPR{T}, width) where T <: Union{CSTParser.Using,CSTParser.Import,CSTParser.Export}
    s = pretty(x.args[1], width)
    for a in x.args[2:end]
        if (a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA) || (a isa CSTParser.OPERATOR && a.kind == Tokens.COLON)
            s *= pretty(a, width) * ws(1)
        else
            s *= pretty(a, width)
        end
    end
    s * NL
end

function pretty(x::T, width) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    #= s = CSTParser.defines_function(x) ? NL : "" =#
    s = ""
    if CSTParser.precedence(x.op) in (8,13,14,16)
        s *= pretty(x.arg1, width)
        s *= pretty(x.op, width)
        s *= pretty(x.arg2, width)
    else
        s *= pretty(x.arg1, width)
        s *= ws(1) * pretty(x.op, width) * ws(1)
        s *= pretty(x.arg2, width)
    end
    #= CSTParser.defines_function(x) && (s *= NL) =#
    s
end

function pretty(x::CSTParser.ConditionalOpCall, width)
    s = pretty(x.cond, width)
    s *= ws(1) * pretty(x.op1, width) * ws(1)
    s *= pretty(x.arg1, width)
    s *= ws(1) * pretty(x.op2, width) * ws(1)
    s * pretty(x.arg2, width)
end

function pretty(x::CSTParser.WhereOpCall, width)
    s = pretty(x.arg1, width)
    s *= ws(1) * pretty(x.op, width) * ws(1)
    s * pretty(x.args, width)
end

function pretty(x::CSTParser.EXPR{CSTParser.Begin}, width)
    s = pretty(x.args[1], width)
    s *= pretty(x.args[2], width; indent=true)
    s * pretty(x.args[3], width)
end

function pretty(x::CSTParser.EXPR{CSTParser.Quote}, width)
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.QUOTE
        s = pretty(x.args[1])
        s *= length(x.args[2]) == 0 ? " " : NL * pretty(x.args[2], width; indent=true)
        return s * pretty(x.args[3], width)
    end
    pretty(x.args, width)
end

function pretty(x::CSTParser.EXPR{CSTParser.Let}, width)
    s = ""
    if length(x.args) > 3
        s *= pretty(x.args[1:2], width)
        s *= length(x.args[3]) == 0 ? " " : NL * pretty(x.args[3], width; indent=true)
        s *= pretty(x.args[4], width)
    else
        s *= pretty(x.args[1], width)
        s *= length(x.args[2]) == 0 ? " " : NL * pretty(x.args[2], width; indent=true)
        s *= pretty(x.args[3], width)
    end
    s
end

function pretty(x::CSTParser.EXPR{CSTParser.If}, width)
    s = pretty(x.args[1:2], width) * NL
    s *= pretty(x.args[3], width; indent=true)
    s *= pretty(x.args[4], width)
    if length(x.args) > 4
        s *= NL
        s *= pretty(x.args[5], width; indent=true)
        s *= pretty(x.args[6], width)
    end
    s
end


function pretty(x::CSTParser.EXPR{CSTParser.Comparison}, width)
    s = ""
    for a in x
        if a isa CSTParser.OPERATOR
            s *= ws(1) * pretty(a, width) * ws(1)
        else
            s *= pretty(a, width)
        end
    end
    s
end

function pretty(x::CSTParser.EXPR{T}, width) where T <: Union{CSTParser.TupleH,CSTParser.ChainOpCall,CSTParser.Call,CSTParser.Vect}
    s = ""
    for a in x
        if a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA
            s *= pretty(a, width) * ws(1)
        else
            s *= pretty(a, width)
        end
    end
    s
end
