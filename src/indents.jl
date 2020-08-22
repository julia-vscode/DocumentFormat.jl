const INDENT = 4

mutable struct IndentState
    indent::Int
    edits
end

function indent_pass(x, state)
    if typof(x) === CSTParser.FileH
        if x.args isa Vector{EXPR}
            for a in x.args
                check_indent(a, state)
                indent_pass(a, state)
            end
        end
    elseif typof(x) === CSTParser.Begin || (typof(x) === CSTParser.Quote && typof(x.args[1]) === CSTParser.KEYWORD && kindof(x.args[1]) == Tokens.QUOTE)
        state.offset += x.args[1].fullspan
        state.edits.indent += 1
        if x.args isa Vector{EXPR} && length(x.args) > 1 && x.args[2].args isa Vector{EXPR}
            for a in x.args[2].args
                check_indent(a, state)
                indent_pass(a, state)
            end
        end
        state.edits.indent -= 1
        check_indent(x.args[3], state)
        state.offset += x.args[3].fullspan
    elseif typof(x) in (CSTParser.FunctionDef, CSTParser.Macro, CSTParser.For, CSTParser.While, CSTParser.Struct)
        state.offset += x.args[1].fullspan + x.args[2].fullspan
        if typof(x.args[3]) === CSTParser.Block
            state.edits.indent += 1
            if x.args[3].args isa Vector{EXPR}
                for a in x.args[3].args
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x.args[4], state)
            state.offset += x.args[4].fullspan
        else
            check_indent(x.args[3], state)
            state.offset += x.args[3].fullspan
        end
    elseif typof(x) === CSTParser.Do
        state.offset += x.args[1].fullspan + x.args[2].fullspan + x.args[3].fullspan
        if typof(x.args[4]) === CSTParser.Block
            state.edits.indent += 1
            if x.args[4].args isa Vector{EXPR}
                for a in x.args[4].args
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x.args[5], state)
            state.offset += x.args[5].fullspan
        else
            check_indent(x.args[4], state)
            state.offset += x.args[4].fullspan
        end
    elseif typof(x) === CSTParser.MacroCall
        if typof(x.args[1]) === CSTParser.GlobalRefDoc
            state.offset += x.args[1].fullspan

            doc = x.args[2]
            doc_strs = split(str_value(doc), "\n")

            state.offset += 4
            for (i, s) in enumerate(doc_strs)
                # Skip indenting lines of "".
                # The final "" is associated with identing the
                # trailing docstring triple quote
                if s == "" && i != length(doc_strs)
                    state.offset += 1
                else
                    a = CSTParser.mLITERAL(length(s) + 1, length(s), String(s), Tokens.STRING)
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.offset += 3

            check_indent(x.args[3], state)
            indent_pass(x.args[3], state)
        else
            for a in x.args
                indent_pass(a, state)
            end
        end
    elseif typof(x) === CSTParser.Mutable
        state.offset += x.args[1].fullspan + x.args[2].fullspan + x.args[3].fullspan
        if typof(x.args[4]) === CSTParser.Block
            state.edits.indent += 1
            if x.args[4].args isa Vector{EXPR}
                for a in x.args[4].args
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x.args[5], state)
            state.offset += x.args[5].fullspan
        else
            check_indent(x.args[3], state)
            state.offset += x.args[4].fullspan
        end
    elseif typof(x) === CSTParser.Try
        state.offset += x.args[1].fullspan

        state.edits.indent += 1
        if x.args[2].args isa Vector{EXPR}
            for a in x.args[2].args
                check_indent(a, state)
                indent_pass(a, state)
            end
        end
        state.edits.indent -= 1
        check_indent(x.args[3], state)
        state.offset += x.args[3].fullspan + x.args[4].fullspan

        state.edits.indent += 1
        if x.args isa Vector{EXPR} && length(x.args) >= 5 && x.args[5].args isa Vector{EXPR}
            for a in x.args[5].args
                check_indent(a, state)
                indent_pass(a, state)
            end
        end
        state.edits.indent -= 1
        check_indent(x.args[6], state)
        state.offset += x.args[6].fullspan

        if length(x) == 8
            state.edits.indent += 1
            if x.args[7].args isa Vector{EXPR}
                for a in x.args[7].args
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x.args[8], state)
            state.offset += x.args[8].fullspan
        end

    elseif typof(x) === CSTParser.If
        if typof(first(x.args)) === CSTParser.KEYWORD && kindof(first(x.args)) == Tokens.IF
            state.offset += x.args[1].fullspan + x.args[2].fullspan
            state.edits.indent += 1
            if x.args[3].args isa Vector{EXPR}
                for a in x.args[3].args
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x.args[4], state)
            state.offset += x.args[4].fullspan
            if length(x.args) > 4
                state.edits.indent += 1
                if x.args[5].args isa Vector{EXPR}
                    for a in x.args[5].args
                        check_indent(a, state)
                        indent_pass(a, state)
                    end
                end
                state.edits.indent -= 1
                check_indent(x.args[6], state)
                state.offset += x.args[6].fullspan
            end
        else
            state.offset += x.args[1].fullspan
            if x.args[2].args isa Vector{EXPR}
                for a in x.args[2].args
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            if length(x.args) > 2
                state.edits.indent -= 1
                check_indent(x.args[3], state)
                state.offset += x.args[3].fullspan
                state.edits.indent += 1
                if x.args[4].args isa Vector{EXPR}
                    for a in x.args[4].args
                        check_indent(a, state)
                        indent_pass(a, state)
                    end
                end
            end
        end
    elseif typof(x) === CSTParser.Let
        if length(x.args) > 3
            state.offset += x.args[1].fullspan + x.args[2].fullspan
            state.edits.indent += 1
            if x.args[3].args isa Vector{EXPR}
                for a in x.args[3].args
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x.args[4], state)
            state.offset += x.args[4].fullspan
        else
            state.offset += x.args[1].fullspan
            state.edits.indent += 1
            if x.args[2].args isa Vector{EXPR}
                for a in x.args[2].args
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x.args[3], state)
            state.offset += x.args[3].fullspan
        end

    elseif typof(x) in (CSTParser.IDENTIFIER, CSTParser.OPERATOR, CSTParser.KEYWORD, CSTParser.PUNCTUATION, CSTParser.LITERAL)
        state.offset += x.fullspan
    else
        if x.args isa Vector{EXPR}
            for a in x.args
                indent_pass(a, state)
            end
        end
    end
    state
end

function check_indent(x, state)
    for (l, i) in state.lines
        if state.offset == l + i
            if state.edits.indent * INDENT != i
                #= @info CSTParser.str_value(CSTParser.get_name(x)), state.edits.indent*INDENT, i, state.offset =#
                push!(state.edits.edits, (l, state.edits.indent * INDENT - i))
            end
        end
    end
end

function indents(text, opts)
    x = CSTParser.parse(text, true)

    state = indent_pass(x, State(0, IndentState(0, []), opts, text, get_lines(text)))

    sort!(state.edits.edits, lt=(a, b) -> a[1] < b[1], rev=true)
    for (l, d) in state.edits.edits
        if d > 0
            text = string(text[1:l], " "^d, text[l + 1:end])
        else
            text = string(text[1:l], text[l + 1 - d:end])
        end
    end
    return text
end
