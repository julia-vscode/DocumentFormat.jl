function format(x::EXPR{T}, F::FormatState) where T <: Union{UnaryOpCall,UnarySyntaxOpCall}
    offset = F.offset
    no_trailing_ws(x.args[1], F)
    format(x.args[1], F)
    F.offset = offset + x.args[1].fullspan
    format(x.args[2], F)
    F.offset += x.args[2].fullspan
end

function format(x::EXPR{T}, F::FormatState) where T <: Union{BinaryOpCall,BinarySyntaxOpCall}
    if x.args[2] isa EXPR{cst.OPERATOR{cst.WhereOp,Tokens.WHERE,false}}
        return formatwhere(x, F)
    end
    ws = !(x.args[2] isa EXPR{OP} where OP <: Union{cst.OPERATOR{cst.ColonOp},cst.OPERATOR{cst.PowerOp},cst.OPERATOR{cst.DeclarationOp},cst.OPERATOR{cst.DotOp}} && !(x.args[2] isa EXPR{cst.OPERATOR{cst.AnonFuncOp,Tokens.ANON_FUNC,false}}))

    offset = F.offset
    ws ? trailing_ws(x.args[1], F) : no_trailing_ws(x.args[1], F)
    format(x.args[1], F)

    F.offset = offset + x.args[1].fullspan
    ws ? trailing_ws(x.args[2], F) : no_trailing_ws(x.args[2], F)
    format(x.args[2], F)
    
    F.offset = offset + x.args[1].fullspan + x.args[2].fullspan
    format(x.args[3], F)
end

function formatwhere(x, F::FormatState)
    offset = F.offset
    trailing_ws(x.args[1], F)
    format(x.args[1], F)

    F.offset = offset + x.args[1].fullspan
    trailing_ws(x.args[2], F)

    F.offset = offset + x.args[1].fullspan + x.args[2].fullspan
    nargs = length(x.args)
    for i = 3:length(x.args)
        a = x.args[i]
        offset = F.offset
        if i < nargs
            no_trailing_ws(a, F)
            format(a, F)
        end
        F.offset = offset + a.fullspan
    end
end

function format(x::EXPR{CSTParser.ConditionalOpCall}, F::FormatState)
    for (i, a) in enumerate(x.args)
        offset = F.offset
        i != 5 && trailing_ws(a, F)
        if iseven(i)
            format(a, F)
        end
        F.offset = offset + a.fullspan
    end
end

function format(x::EXPR{CSTParser.ColonOpCall}, F::FormatState)
    for (i, a) in enumerate(x.args)
        offset = F.offset
        i != 5 && no_trailing_ws(a, F)
        if iseven(i)
            format(a, F)
        end
        F.offset = offset + a.fullspan
    end
end
