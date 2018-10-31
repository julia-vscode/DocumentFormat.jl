#= text(s::AbstractString, cont=false) = CSTParser.parse(s, cont) =#
nest(i::Int, x::CSTParser.AbstractEXPR) = nothing

#= layout(x::CSTParser.LITERAL; in_doc=false) = x.kind == Tokens.STRING && !in_doc ? string("\"", x.val, "\"") : x.val =#

layout(x::CSTParser.LeafNode) = CSTParser.str_value(x)
#= layout(x::EXPR{MacroName}) = string(Expr(x)) =#

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
layout(x::CSTParser.AbstractEXPR) = mapreduce(layout, *, x; init="")
# function layout(n::Nest{T}) where {T<:CSTParser.AbstractEXPR}
# end

###
### FDOC
###

nil() = CSTParser.NOTHING
line() = CSTParser.LITERAL(1, 1:1, "\n", Tokens.NEWLINE_WS)
semicolon() = CSTParser.LITERAL(1, 1:1, ";", Tokens.SEMICOLON)
semicolon_ws() = CSTParser.LITERAL(1, 1:1, ";", Tokens.SEMICOLON_WS)
wsi(n::Int) = CSTParser.LITERAL(n, 1:n, repeat(" ", n), Tokens.WHITESPACE)

fdoc(x::CSTParser.LeafNode) = nothing
function fdoc(x::CSTParser.AbstractEXPR)
    for a in x
        fdoc(a)
    end
end


function fdoc(x::CSTParser.EXPR{T}) where T <: Union{CSTParser.FunctionDef,CSTParser.Macro,CSTParser.For,CSTParser.While,CSTParser.Struct}
    args = [fdoc(x.args[1]), wsi(1), fdoc(x.args[2]), line()]
    if x.args[3] isa CSTParser.EXPR{CSTParser.Block}
        push!(args, fdoc(x.args[3]))
        push!(args, fdoc(x.args[4]))
        push!(args, line())
    else
        push!(args, fdoc(x.args[3]))
        push!(args, line())
    end
    x.args = args
end

function fdoc(x::CSTParser.EXPR{CSTParser.Block})
    args = []
    for a in x.args
        push!(args, fdoc(a))
        push!(args, line())
    end
    x.args = args
end

function fdoc(x::CSTParser.EXPR{CSTParser.Mutable})
    args = [line(), fdoc(x.args[1]), wsi(1), fdoc(x.args[2]), wsi(1), fdoc(x.args[3]), line()]
    if x.args[4] isa CSTParser.EXPR{CSTParser.Block}
        push!(args, fdoc(x.args[4]))
        push!(args, line())
        push!(args, fdoc(x.args[5]))
        push!(args, line())
    else
        push!(args, fdoc(x.args[4]))
        push!(args, line())
    end
    x.args = args
end

function fdoc(x::CSTParser.EXPR{T}) where T <: Union{CSTParser.Using,CSTParser.Import}
    args = []
    for (i, a) in enumerate(x.args)
        if i == 1
            push!(args, fdoc(a))
            push!(args, wsi(1))
            continue
        end
        if (a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA) || (a isa CSTParser.OPERATOR && a.kind == Tokens.COLON)
            push!(args, fdoc(a))
            push!(args, wsi(1))
        else
            push!(args, wsi(1))
            push!(args, fdoc(a))
        end
    end
    x.args = args
end



function fdoc(x::CSTParser.EXPR{CSTParser.Begin})
    args = []
    push!(args, fdoc(x.args[1]))
    push!(args, line())
    push!(args, fdoc(x.args[2]))
    push!(args, fdoc(x.args[3]))
    push!(args, line())
    x.args = args
end

function fdoc(x::CSTParser.EXPR{CSTParser.Quote})
    if x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.QUOTE
        args = []
        push!(args, fdoc(x.args[1]))
        push!(args, line())
        push!(args, fdoc(x.args[2]))
        push!(args, fdoc(x.args[3]))
        push!(args, line())
        x.args = args
        return
    end
    x.args = fdoc.(x.args)
end

function fdoc(x::CSTParser.EXPR{CSTParser.Let})
    args = []
    if length(x.args) > 3
        push!(args, fdoc(x.args[1]))
        push!(args, wsi(1))
        push!(args, fdoc(x.args[2]))
        push!(args, line())
        push!(args, fdoc(x.args[3]))
        push!(args, fdoc(x.args[4]))
        push!(args, line())
    else
        push!(args, fdoc(x.args[1]))
        push!(args, line())
        push!(args, fdoc(x.args[2]))
        push!(args, fdoc(x.args[3]))
        push!(args, line())
    end
    x.args = args
end

fdoc(x::CSTParser.EXPR{CSTParser.Return}) = x.args = [fdoc(x.args[1]), wsi(1), fdoc(x.args[2])]
fdoc(x::CSTParser.EXPR{CSTParser.Parameters}) = x.args = [semicolon(), wsi(1), fdoc.(x.args)...]

function fdoc(x::CSTParser.EXPR{CSTParser.Comparison})
    args = []
    for a in x
        if a isa CSTParser.OPERATOR
            push!(args, wsi(1))
            push!(args, fdoc(a))
            push!(args, wsi(1))
        else
            push!(args, fdoc(a))
        end
    end
    x.args = args
end

function fdoc(x::CSTParser.EXPR{T}) where T <: Union{CSTParser.TupleH,CSTParser.ChainOpCall,CSTParser.Call}
    args = []
    for a in x
        if a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA
            push!(args, fdoc(a))
            push!(args, wsi(1))
        else
            push!(args, fdoc(a))
        end
    end
    x.args = args
end

function fdoc(x::CSTParser.EXPR{CSTParser.StringH})
    args = []
    x.args = args
end

function fdoc(x::CSTParser.EXPR{CSTParser.MacroCall})
    if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        s = "\"\"\"\n"
        s *= fdoc(x.args[2]; in_doc=true)
        s *= "\"\"\"\n"
        s *= fdoc(x.args[3])
        return s
    end
    s = fdoc(x.args[1])
    for a in x.args[2:end]
        s *= " " * fdoc(a)
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

#= fdoc(x::CSTParser.UnaryOpCall) = fdoc(x.op) * fdoc(x.arg) =#
#= fdoc(x::CSTParser.UnarySyntaxOpCall) = fdoc(x.arg1) * fdoc(x.arg2) =#
#
#= function fdoc(x::CSTParser.ConditionalOpCall) =#
#=     s = fdoc(x.cond) =#
#=     s *= " " * fdoc(x.op1) * " " =#
#=     s *= fdoc(x.arg1) =#
#=     s *= " " * fdoc(x.op2) * " " =#
#=     s *= fdoc(x.arg2) =#
#=     s =#
#= end =#
#=  =#
#= function fdoc(x::CSTParser.WhereOpCall) =#
#=     s = fdoc(x.arg1) =#
#=     s *= " " * fdoc(x.op) * " " =#
#=     s *= mapreduce(layout, *, x.args; init="") =#
#=     s =#
#= end =#

function fdoc(x::T) where T <: Union{CSTParser.BinaryOpCall,CSTParser.BinarySyntaxOpCall}
    #= s = CSTParser.defines_function(x) ? "\n" : "" =#
    s = ""
    if CSTParser.precedence(x.op) in (8,13,14,16)
        s *= fdoc(x.arg1)
        s *= fdoc(x.op)
        s *= fdoc(x.arg2)
    else
        s *= fdoc(x.arg1)
        s *= " " * fdoc(x.op) * " "
        s *= fdoc(x.arg2)
    end
    if CSTParser.defines_function(x)
        s *= "\n"
    end
    s
end
