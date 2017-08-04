const IndentEXPR = Union{cst.If,cst.Mutable,cst.Try}

function indent(F::FormatState)
    F.indent += 1
end

function deindent(F::FormatState)
    F.indent -= 1
end


function format(x::EXPR{T}, F::FormatState) where T <: Union{cst.Begin,cst.Quote}
    iml = is_multi_line(x, F)
    F.offset += x.args[1].fullspan
    iml && indent(F)
    for a in x.args[2].args
        iml && check_indent(F)
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    iml && deindent(F)
    iml && check_indent(F)
end

function format(x::EXPR{T}, F::FormatState) where T <: Union{cst.FunctionDef,cst.Macro,cst.Struct,cst.For,cst.While}
    iml = is_multi_line(x, F)
    
    offset = F.offset
    F.offset = x.args[1].fullspan
    format(x.args[2], F)

    F.offset = offset + x.args[1].fullspan + x.args[2].fullspan
    iml && indent(F)
    for a in x.args[3].args
        iml && check_indent(F)
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    iml && deindent(F)
    iml && check_indent(F)
end


function format(x::EXPR{cst.Let}, F::FormatState) 
    iml = is_multi_line(x, F)
    nargs = length(x.args)
    for i = 1:nargs - 2
        a = x.args[i]
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    iml && indent(F)
    for a in x.args[nargs - 1].args
        iml && check_indent(F)
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    iml && deindent(F)
    iml && check_indent(F)
end

function format(x::EXPR{cst.Try}, F::FormatState)
    iml = is_multi_line(x, F)

    F.offset += x.args[1].fullspan
    iml && indent(F)
    for a in x.args[2].args
        iml && check_indent(F)
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end

    iml && deindent(F)
    iml && check_indent(F)
    if x.args[3] isa EXPR{cst.KEYWORD{Tokens.CATCH}}
        F.offset += x.args[3].fullspan
        offset = F.offset
        format(x.args[4], F)
        F.offset = offset + x.args[4].fullspan

        iml && indent(F)
        for a in x.args[5].args
            iml && check_indent(F)
            offset = F.offset
            format(a, F)
            F.offset = offset + a.fullspan
        end
        iml && deindent(F)
        iml && check_indent(F)
    end
end



# function format(x::EXPR{cst.If}, F::FormatState)
#     iml = is_multi_line(x, F)
#     for (i, a) in enumerate(x.args)
#         offset = F.offset
#         if a isa EXPR{cst.KEYWORD{Tokens.IF}} || a isa EXPR{cst.KEYWORD{Tokens.ELSE}} || a isa EXPR{cst.KEYWORD{Tokens.ELSEIF}}
#             iml && indent(F)
#         elseif a isa EXPR{cst.KEYWORD{Tokens.END}}
#         elseif a isa EXPR{cst.Block}
#             for (j, b) in enumerate(a.args)
#                 iml && check_indent(F)
#                 offset1 = F.offset
#                 format(a, F)
#                 F.offset = offset1 + b.fullspan
#             end
#             F.offset = offset
#             iml && deindent(F)
#         end
#         format(a, F)
#         F.offset = offset + a.fullspan
#     end
# end

# function format(x::EXPR{cst.If}, F::FormatState)
#     nargs = length(x.args)
#     if nargs == 4
#         formatif4(x, F)
#     elseif nargs == 6
#         formatif6(x, F)
#     elseif nargs == 2
#         formatif2nested(x, F)
#     elseif nargs == 4
#         formatif4nested(x, F)
#     end
# end

function formatif2nested(x::EXPR{cst.If}, F::FormatState)
    iml = is_multi_line(x, F)
    offset = F.offset
    format(x.args[1], F)

    F.offset = offset + x.args[1].fullspan
    # iml && indent(F)
    for a in x.args[2].args
        iml && check_indent(F)
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    # iml && deindent(F)
    # iml && check_indent(F)
end

function formatif4nested(x::EXPR{cst.If}, F::FormatState)
    iml = is_multi_line(x, F)
    haselse = length(x.args) == 6
    
    offset = F.offset
    format(x.args[1], F)

    F.offset = offset + x.args[1].fullspan
    # iml && indent(F)
    for a in x.args[2].args
        iml && check_indent(F)
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    iml && deindent(F)
    iml && check_indent(F)

    F.offset += x.args[3].fullspan
    iml && indent(F)
    for a in x.args[4].args
        iml && check_indent(F)
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    # iml && deindent(F)
    # iml && check_indent(F)
    
end


function formatif4(x::EXPR{cst.If}, F::FormatState)
    iml = is_multi_line(x, F)
    
    offset = F.offset
    F.offset += x.args[1].fullspan
    format(x.args[2], F)

    F.offset = offset + x.args[1].fullspan + x.args[2].fullspan
    iml && indent(F)
    for a in x.args[3].args
        iml && check_indent(F)
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    iml && deindent(F)
    iml && check_indent(F)
end


function formatif6(x::EXPR{cst.If}, F::FormatState)
    iml = is_multi_line(x, F)
    
    offset = F.offset
    F.offset += x.args[1].fullspan
    format(x.args[2], F)

    F.offset = offset + x.args[1].fullspan + x.args[2].fullspan
    iml && indent(F)
    for a in x.args[3].args
        iml && check_indent(F)
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    info(F.offset)
    iml && deindent(F)
    iml && check_indent(F)

    F.offset += x.args[4].fullspan
    iml && indent(F)
    for a in x.args[5].args
        iml && check_indent(F)
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    iml && deindent(F)
    iml && check_indent(F)
end
