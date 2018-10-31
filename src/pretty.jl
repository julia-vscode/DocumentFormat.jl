
const WS_INDENT = repeat(" ", 4);

pretty(x::CSTParser.AbstractEXPR) = CSTParser.str_value(x)
pretty(x::CSTParser.LITERAL; in_doc=false) = x.kind == Tokens.STRING && !in_doc ? string("\"", x.val, "\"") : x.val

function pretty(x::CSTParser.PUNCTUATION)
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

pretty(x::CSTParser.KEYWORD) = x.kind |> string |> lowercase
pretty(x::CSTParser.EXPR{T}) where {T} = mapreduce(pretty, *, x.args; init="")
#= pretty(x::CSTParser.AbstractEXPR) = mapreduce(pretty, *, x; init="") =#

function pretty(x::CSTParser.EXPR{CSTParser.StringH}; in_doc=false)
    s = in_doc ? "" : "\""
    for a in x
        if a isa CSTParser.LITERAL
            s *= pretty(a; in_doc=in_doc)
        else
            s *= pretty(a)
        end
    end
    s *= in_doc ? "" : "\""
end

function pretty(x::CSTParser.EXPR{CSTParser.MacroCall})
    if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        s = "\"\"\"\n"
        s *= pretty(x.args[2]; in_doc=true)
        s *= "\"\"\"\n"
        s *= pretty(x.args[3])
        return s
    end
    s = pretty(x.args[1])
    for a in x.args[2:end]
        s *= " " * pretty(a)
    end
    s
end

function pretty(x::CSTParser.EXPR{T}) where T <: Union{CSTParser.FunctionDef,CSTParser.Macro,CSTParser.For,CSTParser.While,CSTParser.Struct}
    s = "\n"
    s *= pretty(x.args[1])
    s *= " " * pretty(x.args[2]) * "\n"
    if x.args[3] isa CSTParser.EXPR{CSTParser.Block}
        for a in x.args[3]
            s *= WS_INDENT * pretty(a) * "\n"
        end
        s *= pretty(x.args[4]) * "\n"
    else
        s *= pretty(x.args[3]) * "\n"
    end
    s
end

function pretty(x::CSTParser.EXPR{CSTParser.Mutable})
    s = pretty(x.args[1])
    s *= " " * pretty(x.args[2])
    s *= " " * pretty(x.args[3]) * "\n"
    if x.args[4] isa CSTParser.EXPR{CSTParser.Block}
        for a in x.args[4]
            s *= WS_INDENT * pretty(a) * "\n"
        end
        s *= pretty(x.args[5]) * "\n"
    else
        s *= pretty(x.args[4]) * "\n"
    end
    s
end

#= function pretty(x::CSTParser.EXPR{CSTParser.Try}) =#
#=     s = pretty(x.args[1]) =#
#=     for a in x.args[2] =#
#=         s *= pretty(a) * "\n" =#
#=     end =#
#=     s *= pretty(x.args[3]) * " " * pretty(x.args[4]) =#
#=     for a in x.args[5] =#
#=         s *= pretty(a) * "\n" =#
#=     end =#
#=     s *= pretty(x.args[6]) * "\n" =#
#=     s =#
#= end =#

function pretty(x::CSTParser.EXPR{T}) where T <: Union{CSTParser.Using,CSTParser.Import}
    s = ""
    for (i, a) in enumerate(x.args)
        if i == 1
            s *= string(pretty(a), " ")
            continue
        end
        if (a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA) || (a isa CSTParser.OPERATOR && a.kind == Tokens.COLON)
            s *= string(pretty(a), " ")
        else
            s *= string(" ", pretty(a))
        end
    end
    return s * "\n"
end

function pretty(x::T) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    #= s = CSTParser.defines_function(x) ? "\n" : "" =#
    s = ""
    if CSTParser.precedence(x.op) in (8,13,14,16)
        s *= pretty(x.arg1)
        s *= pretty(x.op)
        s *= pretty(x.arg2)
    else
        s *= pretty(x.arg1)
        s *= " " * pretty(x.op) * " "
        s *= pretty(x.arg2)
    end
    if CSTParser.defines_function(x)
        s *= "\n"
    end
    s
end

function pretty(x::CSTParser.ConditionalOpCall)
    s = pretty(x.cond)
    s *= " " * pretty(x.op1) * " "
    s *= pretty(x.arg1)
    s *= " " * pretty(x.op2) * " "
    s *= pretty(x.arg2)
    s
end

function pretty(x::CSTParser.WhereOpCall)
    s = pretty(x.arg1)
    s *= " " * pretty(x.op) * " "
    s *= mapreduce(pretty, *, x.args; init="")
    s
end

function pretty(x::CSTParser.EXPR{CSTParser.Begin})
    s = pretty(x.args[1]) * "\n"
    for a in x.args[2]
        s *= WS_INDENT * pretty(a) * "\n"
    end
    s *= pretty(x.args[3])  * "\n"
    s
end

function pretty(x::CSTParser.EXPR{CSTParser.Quote})
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.QUOTE
        s = pretty(x.args[1]) * "\n"
        for a in x.args[2]
            s *= WS_INDENT * pretty(a) * "\n"
        end
        s *= pretty(x.args[3]) * "\n"
        return s
    end
    mapreduce(pretty, *, x.args; init="")
end

function pretty(x::CSTParser.EXPR{CSTParser.Let})
    s = ""
    if length(x.args) > 3
        s *= pretty(x.args[1]) * " " * pretty(x.args[2])
        s *= "\n"
        for a in x.args[3]
            s *= pretty(a) * "\n"
        end
        s *= pretty(x.args[4]) * "\n"
    else
        s *= pretty(x.args[1])
        s *= "\n"
        for a in x.args[2]
            s *= pretty(a) * "\n"
        end
        s *= pretty(x.args[3]) * "\n"
    end
    s
end

pretty(x::CSTParser.UnaryOpCall) = pretty(x.op) * pretty(x.arg)
pretty(x::CSTParser.UnarySyntaxOpCall) = pretty(x.arg1) * pretty(x.arg2)
pretty(x::CSTParser.EXPR{CSTParser.Return}) = pretty(x.args[1]) * " " * pretty(x.args[2])
pretty(x::CSTParser.EXPR{CSTParser.Parameters}) = "; " * mapreduce(pretty, *, x.args; init="")

function pretty(x::CSTParser.EXPR{CSTParser.Comparison})
    s = ""
    for a in x
        if a isa CSTParser.OPERATOR
            s *= " " * pretty(a) * " "
        else
            s *= pretty(a)
        end
    end
    s
end

function pretty(x::CSTParser.EXPR{T}) where T <: Union{CSTParser.TupleH,CSTParser.ChainOpCall,CSTParser.Call}
    s = ""
    for a in x
        if a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA
            s *= pretty(a) * " "
        else
            s *= pretty(a)
        end
    end
    s
end

### Doc

function fdoc(x)
end
