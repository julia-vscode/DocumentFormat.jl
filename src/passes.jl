function operator_pass(x, state)
    if x isa CSTParser.BinaryOpCall || x isa CSTParser.BinarySyntaxOpCall
        if CSTParser.precedence(x.op) in (8,13,14,16)
            ensure_no_space_after(x.arg1, state, state.offset)
            ensure_no_space_after(x.op, state, state.offset + x.arg1.fullspan)
        else
            ensure_single_space_after(x.arg1, state, state.offset)
            ensure_single_space_after(x.op, state, state.offset + x.arg1.fullspan)
        end
    elseif x isa CSTParser.WhereOpCall
        ensure_single_space_after(x.op, state, state.offset + x.arg1.fullspan)
        n = length(x.args)
        offset = state.offset + x.arg1.fullspan + x.op.fullspan
        for (i,a) in enumerate(x.args)
            if i != n
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    elseif x isa CSTParser.EXPR{CSTParser.ColonOpCall}
        offset = state.offset
        n = length(x.args)
        for (i,a) in enumerate(x.args)
            if i != n
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    elseif x isa CSTParser.EXPR{CSTParser.ChainOpCall} || x isa CSTParser.EXPR{CSTParser.Comparison}
        offset = state.offset
        n = length(x.args)
        for (i,a) in enumerate(x.args)
            if i != n
                ensure_single_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    end
end

function tuple_pass(x, state)
    if x isa CSTParser.EXPR{CSTParser.TupleH}
        offset = state.offset
        n = length(x)
        for (i, a) in enumerate(x)
            if a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA && i !=n && !(x.args[i+1] isa CSTParser.PUNCTUATION)
                ensure_single_space_after(a, state, offset)
            elseif i != n
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    end
end

function curly_pass(x, state)
    if x isa CSTParser.EXPR{CSTParser.Curly}
        offset = state.offset
        n = length(x)
        for (i, a) in enumerate(x)
            if i != n
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    end
end

function call_pass(x, state)
    if x isa CSTParser.EXPR{CSTParser.Call}
        offset = state.offset + x.args[1].fullspan
        n = length(x)
        for (i, a) in enumerate(x)
            i == 1 && continue
            if a isa CSTParser.PUNCTUATION && a.kind == Tokens.COMMA
                ensure_single_space_after(a, state, offset)
            # elseif a isa CSTParser.EXPR{CSTParser.Parameters}
            elseif i != n && !(x.args[i + 1] isa CSTParser.EXPR{CSTParser.Parameters})
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    elseif x isa CSTParser.EXPR{CSTParser.Kw}
        ensure_single_space_after(x.args[1], state, state.offset)
        ensure_single_space_after(x.args[2], state, state.offset + x.args[1].fullspan)
    end
end

function forloop_pass(x, state)
    if x isa CSTParser.EXPR{CSTParser.For}
        offset = state.offset + x.args[1].fullspan
        for a in x.args[2]
            # convert iter = I into iter in I
            if a isa CSTParser.BinarySyntaxOpCall && CSTParser.is_eq(a.op)
                offset += a.arg1.fullspan
                push!(state.edits, Edit(offset+1:offset+2, "in "))
                offset += a.op.fullspan
                offset += a.arg2.fullspan
            else
                offset += a.fullspan
            end
        end
    end
end

function doc_pass(x, state)
    if x isa CSTParser.EXPR{CSTParser.MacroCall} && x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
        # if the docstring is global align it to:
        #
        # """
        # doc
        # """
        #
        # cases:
        #
        # """
        # doc"""
        #
        # """doc
        # """
        #
        # """doc"""
        offset = state.offset + x.args[1].fullspan
        doc = x.args[2]
        doc_fullspan = offset + doc.fullspan
        tq = "\"\"\"\n"
        ltq = length(tq)

        ss = string(tq, strip(doc.val), "\n")
        push!(state.edits, Edit(offset+1:offset+length(ss), ss))
        offset += length(ss)

        # It's possible adding the closing triple quote + newline
        # will extend past the docstring fullspan
        diff = offset + ltq - doc_fullspan
        if diff > 0
            l = ltq - diff
            push!(state.edits, Edit(offset+1:offset+l, tq[1:l]))
            offset += l
            push!(state.edits, Edit(offset, tq[l+1:end]))
        else
            push!(state.edits, Edit(offset+1:offset+ltq, tq))
            offset += ltq
        end

        if offset < doc_fullspan
            push!(state.edits, Edit(offset+1:doc_fullspan, ""))
        end
    end
end
