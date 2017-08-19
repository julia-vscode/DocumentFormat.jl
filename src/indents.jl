function indent(F::FormatState)
    F.indent += 1
end

function deindent(F::FormatState)
    F.indent -= 1
end

function format_block(x, F, doindent)
    doindent && indent(F)
    for a in x.args
        doindent && check_indent(F)
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    doindent && deindent(F)
    doindent && check_indent(F)
end

function format(x::EXPR{T}, F::FormatState) where T <: Union{cst.Begin,cst.Quote,cst.While,cst.FunctionDef,cst.Macro,cst.Struct,cst.Mutable,cst.If}
    doindent = is_multi_line(x, F) && T in F.config.IndentEXPR
    for a in x.args
        offset = F.offset
        if a isa EXPR{cst.Block}
            format_block(a, F, doindent)
            F.offset = offset + a.fullspan
        else
            format(a, F)
            F.offset = offset + a.fullspan
        end
    end
end

function format(x::EXPR{cst.For}, F::FormatState)
    doindent = is_multi_line(x, F) && cst.For in F.config.IndentEXPR
    
    offset = F.offset
    F.offset += x.args[1].fullspan
    format(x.args[2], F)
    F.offset = offset + x.args[1].fullspan + x.args[2].fullspan

    format_block(x.args[3], F, doindent)
end


function format(x::EXPR{cst.Let}, F::FormatState) 
    doindent = is_multi_line(x, F) && cst.Let in F.config.IndentEXPR
    nargs = length(x.args)
    for i = 1:nargs - 2
        a = x.args[i]
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    format_block(x.args[nargs-1], F, doindent)
end

function format(x::EXPR{cst.Try}, F::FormatState)
    doindent = is_multi_line(x, F) && cst.Try in F.config.IndentEXPR
    F.offset += x.args[1].fullspan

    format_block(x.args[2], F, doindent)
    if x.args[3] isa EXPR{cst.KEYWORD{Tokens.CATCH}}
        F.offset += x.args[3].fullspan
        offset = F.offset
        format(x.args[4], F)
        F.offset = offset + x.args[4].fullspan
        format_block(x.args[5], F, doindent)
    end
end

function format(x::EXPR{cst.If}, F::FormatState)
    doindent = is_multi_line(x, F) && cst.If in F.config.IndentEXPR
    nested = !(x.args[1] isa EXPR{cst.KEYWORD{Tokens.IF}})
    for a in x.args
        offset = F.offset
        if a isa EXPR{cst.Block} && !isnestedifblock(a)
            format_block(a, F, doindent)
            F.offset = offset + a.fullspan
        else
            format(a, F)
            F.offset = offset + a.fullspan
        end
    end
end
