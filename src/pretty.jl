# Prettier interface
#
# Doc is a concrete syntax tree (CST.AbstractEXPR)
#
# (<|>) merge :: Doc -> Doc -> Doc
# (<>) concat :: Doc -> Doc -> Doc
# nil :: Doc
# text :: String -> Doc
# line :: Doc
# nest :: Int -> Doc -> Doc
# layout :: Doc -> String
# pretty :: Int -> Doc -> String
# group :: Doc -> Doc
# flatten :: Doc -> Doc
#= merge(x1::T, x2::T) where {T <: CSTParser.AbstractEXPR} = nothing =#
nil() = CSTParser.NOTHING
text(s::AbstractString, cont=false) = CSTParser.parse(s, cont)
line() = CSTParser.LITERAL(1, 1:1, "\n", Tokens.NEWLINE_WS)
nest(i::Int, x::CSTParser.AbstractEXPR) = nothing
layout(x::CSTParser.AbstractEXPR) = CSTParser.str_value(x)
layout(x::CSTParser.LITERAL; in_doc=false) = x.kind == Tokens.STRING && !in_doc ? string("\"", x.val, "\"") : x.val

function layout(x::CSTParser.PUNCTUATION)
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

layout(x::CSTParser.KEYWORD) = x.kind |> string |> lowercase
layout(x::CSTParser.EXPR{T}) where {T} = mapreduce(layout, *, x.args; init="")

function layout(x::CSTParser.EXPR{CSTParser.StringH}; in_doc=false)
    s = in_doc ? "" : "\""
    for a in x
        if a isa CSTParser.LITERAL
            s *= layout(a; in_doc=in_doc)
        else
            s *= layout(a)
        end
    end
    s *= in_doc ? "" : "\""
end

function layout(x::CSTParser.EXPR{CSTParser.MacroCall})
    if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        s = "\"\"\"\n"
        s *= layout(x.args[2]; in_doc=true)
        s *= "\"\"\"\n"
        s *= layout(x.args[3])
        return s
    end
    s = layout(x.args[1])
    for a in x.args[2:end]
        s *= " " * layout(a)
    end
    s
end

function layout(x::CSTParser.EXPR{T}) where T <: Union{CSTParser.FunctionDef,CSTParser.Macro,CSTParser.For,CSTParser.While,CSTParser.Struct}
    s = "\n"
    s *= layout(x.args[1])
    s *= " " * layout(x.args[2]) * "\n"
    if x.args[3] isa CSTParser.EXPR{CSTParser.Block}
        for a in x.args[3]
            s *= layout(a) * "\n"
        end
        s *= layout(x.args[4]) * "\n"
    else
        s *= layout(x.args[3]) * "\n"
    end
    s
end

function layout(x::CSTParser.EXPR{CSTParser.Mutable})
    s = layout(x.args[1])
    s *= " " * layout(x.args[2])
    s *= " " * layout(x.args[3]) * "\n"
    if x.args[4] isa CSTParser.EXPR{CSTParser.Block}
        for a in x.args[4]
            s *= layout(a) * "\n"
        end
        s *= layout(x.args[5]) * "\n"
    else
        s *= layout(x.args[4]) * "\n"
    end
    s
end

#= function layout(x::CSTParser.EXPR{CSTParser.Try}) =#
#=     s = layout(x.args[1]) =#
#=     for a in x.args[2] =#
#=         s *= layout(a) * "\n" =#
#=     end =#
#=     s *= layout(x.args[3]) * " " * layout(x.args[4]) =#
#=     for a in x.args[5] =#
#=         s *= layout(a) * "\n" =#
#=     end =#
#=     s *= layout(x.args[6]) * "\n" =#
#=     s =#
#= end =#

function layout(x::CSTParser.EXPR{T}) where T <: Union{CSTParser.Using,CSTParser.Import}
    s = ""
    for (i, a) in enumerate(x.args)
        if i == 1
            s *= string(layout(a), " ")
            continue
        end
        if (a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA) || (a isa CSTParser.OPERATOR && a.kind == Tokens.COLON)
            s *= string(layout(a), " ")
        else
            s *= string(" ", layout(a))
        end
    end
    return s * "\n"
end

function layout(x::T) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    #= s = CSTParser.defines_function(x) ? "\n" : "" =#
    s = ""
    if CSTParser.precedence(x.op) in (8,13,14,16)
        s *= layout(x.arg1)
        s *= layout(x.op)
        s *= layout(x.arg2)
    else
        s *= layout(x.arg1)
        s *= " " * layout(x.op) * " "
        s *= layout(x.arg2)
    end
    if CSTParser.defines_function(x)
        s *= "\n"
    end
    s
end

function layout(x::CSTParser.ConditionalOpCall)
    s = layout(x.cond)
    s *= " " * layout(x.op1) * " "
    s *= layout(x.arg1)
    s *= " " * layout(x.op2) * " "
    s *= layout(x.arg2)
    s
end

function layout(x::CSTParser.WhereOpCall)
    s = layout(x.arg1)
    s *= " " * layout(x.op) * " "
    s *= mapreduce(layout, *, x.args; init="")
    s
end

function layout(x::CSTParser.EXPR{CSTParser.Begin})
    s = layout(x.args[1]) * "\n"
    for a in x.args[2]
        s *= layout(a) * "\n"
    end
    s *= layout(x.args[3])  * "\n"
    s
end

function layout(x::CSTParser.EXPR{CSTParser.Quote})
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.QUOTE
        s = layout(x.args[1]) * "\n"
        for a in x.args[2]
            s *= layout(a) * "\n"
        end
        s *= layout(x.args[3]) * "\n"
        return s
    end
    mapreduce(layout, *, x.args; init="")
end

function layout(x::CSTParser.EXPR{CSTParser.Let})
    s = ""
    if length(x.args) > 3
        s *= layout(x.args[1]) * " " * layout(x.args[2])
        s *= "\n"
        for a in x.args[3]
            s *= layout(a) * "\n"
        end
        s *= layout(x.args[4]) * "\n"
    else
        s *= layout(x.args[1])
        s *= "\n"
        for a in x.args[2]
            s *= layout(a) * "\n"
        end
        s *= layout(x.args[3]) * "\n"
    end
    s
end

layout(x::CSTParser.UnaryOpCall) = layout(x.op) * layout(x.arg)
layout(x::CSTParser.UnarySyntaxOpCall) = layout(x.arg1) * layout(x.arg2)
layout(x::CSTParser.EXPR{CSTParser.Return}) = layout(x.args[1]) * " " * layout(x.args[2])
layout(x::CSTParser.EXPR{CSTParser.Parameters}) = "; " * mapreduce(layout, *, x.args; init="")

function layout(x::CSTParser.EXPR{CSTParser.Comparison})
    s = ""
    for a in x
        if a isa CSTParser.OPERATOR
            s *= " " * layout(a) * " "
        else
            s *= layout(a)
        end
    end
    s
end

function layout(x::CSTParser.EXPR{T}) where T <: Union{CSTParser.TupleH,CSTParser.ChainOpCall,CSTParser.Call}
    s = ""
    for a in x
        if a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA
            s *= layout(a) * " "
        else
            s *= layout(a)
        end
    end
    s
end

### Doc

function fdoc(x)
end
