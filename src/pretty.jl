INDENT_WIDTH = 4
NL = '\n'
ws(n::Int) = repeat(' ', n)

pretty(x) = pretty(x, 0)
pretty(x::T, width) where {T<:Union{CSTParser.AbstractEXPR, Vector}} = mapreduce(y -> pretty(y, width), *, x; init="")
pretty(x::T, width) where {T <: Union{CSTParser.IDENTIFIER,CSTParser.LITERAL}} = x.val
pretty(x::CSTParser.OPERATOR, width) = string(CSTParser.Expr(x))
pretty(x::CSTParser.EXPR{CSTParser.MacroName}, width) = string(CSTParser.Expr(x))
pretty(x::CSTParser.KEYWORD, width) = x.kind |> string |> lowercase

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
    return ""
end

pretty(x::CSTParser.EXPR{CSTParser.Return}, width) = pretty(x.args[1], width) * ws(1) * pretty(x.args[2], width)
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
    s = ""
    if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        s *= "\"\"\"\n"
        s *= strip(pretty(x.args[2], width; in_doc=true), ['\n']) * NL
        s *= "\"\"\"\n"
        s *= pretty(x.args[3], width)
    else
        s *= pretty(x.args[1], width)
        for a in x.args[2:end]
            s *= ws(1) * pretty(a, width)
        end
    end
    s
end

function pretty(x::CSTParser.EXPR{CSTParser.Block}, width; indent=false)
    s = ""
    indent && (width += INDENT_WIDTH)
    for a in x
        if indent
            s *= join(ws(INDENT_WIDTH) .* split(pretty(a, width), "\n"), "\n") * NL
        else
            s *= pretty(a, width)
        end
    end
    s
end

function pretty(x::CSTParser.EXPR{T}, width) where T <: Union{CSTParser.FunctionDef,CSTParser.Macro,CSTParser.For,CSTParser.While,CSTParser.Struct}
    s = pretty(x.args[1], width) * ws(1) * pretty(x.args[2], width) * NL
    if x.args[3] isa CSTParser.EXPR{CSTParser.Block}
        s *= pretty(x.args[3], width; indent=true)
        s *= pretty(x.args[4], width) * NL
    else
        s *= pretty(x.args[3], width) * NL
    end
    s
end


function pretty(x::CSTParser.EXPR{CSTParser.Mutable}, width)
    s = pretty(x.args[1], width) * ws(1) * pretty(x.args[2], width) * ws(1) * pretty(x.args[3], width) * NL
    if x.args[4] isa CSTParser.EXPR{CSTParser.Block}
        s *= pretty(x.args[4], width; indent=true)
        s *= pretty(x.args[5], width) * NL
    else
        s *= pretty(x.args[4], width) * NL
    end
    s
end

function pretty(x::CSTParser.EXPR{CSTParser.Do}, width)
    s = pretty(x.args[1], width) * ws(1) * pretty(x.args[2], width) * ws(1) * pretty(x.args[3], width) * NL
    if x.args[4] isa CSTParser.EXPR{CSTParser.Block}
        s *= pretty(x.args[4], width; indent=true)
        s *= pretty(x.args[5], width) * NL
    else
        s *= pretty(x.args[4], width) * NL
    end
    s
end

function pretty(x::CSTParser.EXPR{CSTParser.Try}, width)
    s = pretty(x.args[1], width) * NL
    s *= pretty(x.args[2], width; indent=true)
    s *= pretty(x.args[3], width) * ws(1) * pretty(x.args[4], width) * NL
    s *= pretty(x.args[5], width; indent=true)
    s *= pretty(x.args[6]) * NL
    if length(x.args) > 6
        s *= pretty(x.args[7], width; indent=true)
        s *= pretty(x.args[8]) * NL
    end
    s
end

function pretty(x::CSTParser.EXPR{T}, width) where T <: Union{CSTParser.Using,CSTParser.Import}
    s = ""
    for (i, a) in enumerate(x.args)
        if i == 1
            s *= pretty(a, width) * ws(1)
            continue
        end
        if (a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA) || (a isa CSTParser.OPERATOR && a.kind == Tokens.COLON)
            s *= pretty(a, width) * ws(1)
        else
            s *= ws(1) * pretty(a, width)
        end
    end
    return s * NL
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
    CSTParser.defines_function(x) && (s *= NL)
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
    s = pretty(x.args[1], width) * NL
    if x.args[2] isa CSTParser.EXPR{CSTParser.Block}
        s *= pretty(x.args[2], width; indent=true)
    else
        s *= pretty(x.args[2], width)
    end
    s * pretty(x.args[3], width)  * NL
end

function pretty(x::CSTParser.EXPR{CSTParser.Quote}, width)
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.QUOTE
        s = pretty(x.args[1]) * NL
        if x.args[2] isa CSTParser.EXPR{CSTParser.Block}
            s *= pretty(x.args[2], width; indent=true)
        else
            s *= pretty(x.args[2], width)
        end
        return s * pretty(x.args[3], width) * NL
    end
    pretty(x.args, width)
end

function pretty(x::CSTParser.EXPR{CSTParser.Let}, width)
    s = ""
    if length(x.args) > 3
        s *= pretty(x.args[1], width) * ws(1) * pretty(x.args[2], width) * NL
        #= s *= x.args[3] isa CSTParser.EXPR{CSTParser.Block} ? pretty(x.args[3], width; indent=true) : pretty(x.args[3], width) =#
        if x.args[3] isa CSTParser.EXPR{CSTParser.Block}
            s *= pretty(x.args[3], width; indent=true)
        else
            s *= pretty(x.args[3], width)
        end
        s *= pretty(x.args[4], width) * NL
    else
        s *= pretty(x.args[1], width) * NL
        if x.args[2] isa CSTParser.EXPR{CSTParser.Block}
            s *= pretty(x.args[2], width; indent=true)
        else
            s *= pretty(x.args[2], width)
        end
        s *= pretty(x.args[3], width) * NL
    end
    s
end

function pretty(x::CSTParser.EXPR{CSTParser.If}, width)
    s = ""
    if x.args[1] isa CSTParser.KEYWORD && x.args[1] == Tokens.IF
        s *= pretty(x.args[1], width) * ws(1) * pretty(x.args[2], width) * NL
        s *= pretty(x.args[3], width; indent=true)
        s *= pretty(x.args[4], width) * NL
        s *= pretty(x.args[5], width; indent=true)
        s *= pretty(x.args[6], width) * NL
    else
        s *= pretty(x.args[1], width) * NL
        s *= pretty(x.args[2], width; indent=true)
        if length(x.args[4]) > 2
            s *= pretty(x.args[3], width) * NL
            s *= pretty(x.args[4], width; indent=true)
        end
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
