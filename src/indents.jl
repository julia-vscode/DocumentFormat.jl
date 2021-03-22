const INDENT = 4

mutable struct IndentState
    indent::Int
    edits
end

function indent_pass(x, state)
    if headof(x) === :file
        if length(x) > 0
            for a in x
                check_indent(a, state)
                indent_pass(a, state)
            end
        end
    elseif (headof(x) === :block && headof(x[1]) == :BEGIN) || (headof(x) === :quote && headof(x[1]) === :QUOTE)
        state.offset += x[1].fullspan
        state.edits.indent += 1
        if length(x) > 2
            for i = 2:length(x) - 1
                a = x[i]
                check_indent(a, state)
                indent_pass(a, state)
            end
        end
        state.edits.indent -= 1
        check_indent(x[3], state)
        state.offset += x[3].fullspan
    elseif headof(x) == :struct
        state.offset += x[1].fullspan + x.args[2].fullspan
        if length(x.trivia) > 2 #mutable
            state.offset += x.trivia[2].fullspan
        end
        if headof(x.args[3]) === :block && length(x.args[3]) > 0
            state.edits.indent += 1
            for a in x.args[3]
                check_indent(a, state)
                indent_pass(a, state)
            end
            state.edits.indent -= 1
            check_indent(last(x.trivia), state)
            state.offset += last(x.trivia).fullspan
        end
    elseif headof(x) in (:function, :macro, :for, :while)
        state.offset += x[1].fullspan + x[2].fullspan
        if headof(x[3]) === :block
            state.edits.indent += 1
            if length(x[3]) > 0
                for a in x[3]
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x[4], state)
            state.offset += x[4].fullspan
        else
            check_indent(x[3], state)
            state.offset += x[3].fullspan
        end
    elseif headof(x) === :do
        state.offset += x[1].fullspan + x[2].fullspan + x[3][1].fullspan
        if headof(x[3][3]) === :block
            state.edits.indent += 1
            if length(x[3][3]) > 0
                for a in x[3][3]
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x[4], state)
            state.offset += x[4].fullspan
        else
            check_indent(x[4], state)
            state.offset += x[4].fullspan
        end
    elseif headof(x) === :macrocall
        if headof(x[1]) === :globalrefdoc
            state.offset += x[1].fullspan
            doc = x.args[3]
            doc_strs = split(str_value(doc), "\n")
            state.offset += 4 
            for (i, s) in enumerate(doc_strs)
                # Skip indenting lines of "".
                # The final "" is associated with identing the
                # trailing docstring triple quote
                if s == "" && i != length(doc_strs)
                    state.offset += 1
                else
                    a = EXPR(:STRING, length(s) + 1, length(s), String(s))
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.offset += 3

            check_indent(x.args[4], state)
            indent_pass(x.args[4], state)
        elseif length(x) > 0
            for a in x
                indent_pass(a, state)
            end
        end
    elseif headof(x) === :mutable
        state.offset += x[1].fullspan + x[2].fullspan + x[3].fullspan
        if headof(x[4]) === :block
            state.edits.indent += 1
            if length(x[4]) > 0
                for a in x[4]
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x[5], state)
            state.offset += x[5].fullspan
        else
            check_indent(x[3], state)
            state.offset += x[4].fullspan
        end
    elseif headof(x) === :try && length(x) > 5
        state.offset += x[1].fullspan
        state.edits.indent += 1
        if length(x[2]) > 0
            for a in x[2]
                check_indent(a, state)
                indent_pass(a, state)
            end
        end
        state.edits.indent -= 1
        check_indent(x[3], state)
        state.offset += x[3].fullspan + x[4].fullspan

        state.edits.indent += 1
        if length(x[5]) > 0
            for a in x[5]
                check_indent(a, state)
                indent_pass(a, state)
            end
        end
        state.edits.indent -= 1
        check_indent(x[6], state)
        state.offset += x[6].fullspan

        if length(x) == 8
            state.edits.indent += 1
            if length(x[7]) > 0
                for a in x[7]
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x[8], state)
            state.offset += x[8].fullspan
        end
    elseif headof(x) === :if
        if headof(first(x)) == :IF
            state.offset += x[1].fullspan + x[2].fullspan
            state.edits.indent += 1
            if length(x[3]) > 0
                for a in x[3]
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            if length(x) >= 4
                state.edits.indent -= 1
                check_indent(x[4], state)
                state.offset += x[4].fullspan
            end
            if length(x) == 5
                # state.edits.indent += 1
                check_indent(x[5], state)
                state.offset += x[5].fullspan
            elseif length(x) == 6
                state.edits.indent += 1
                for a in x[5]
                    check_indent(a, state)
                    indent_pass(a, state)
                end
                state.edits.indent -= 1
                check_indent(x[6], state)
                state.offset += x[6].fullspan
            end
        else
            state.offset += x[1].fullspan
            if length(x[2]) > 0
                for a in x[2]
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            if length(x) > 2
                state.edits.indent -= 1
                check_indent(x[3], state)
                state.offset += x[3].fullspan
                state.edits.indent += 1
                if length(x[4]) > 0
                    for a in x[4]
                        check_indent(a, state)
                        indent_pass(a, state)
                    end
                end
            end
        end
    elseif headof(x) === :let
        if length(x) > 3
            state.offset += x[1].fullspan + x[2].fullspan
            state.edits.indent += 1
            if length(x[3]) > 0
                for a in x[3]
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x[4], state)
            state.offset += x[4].fullspan
        else
            state.offset += x[1].fullspan
            state.edits.indent += 1
            if length(x[2]) > 0
                for a in x[2]
                    check_indent(a, state)
                    indent_pass(a, state)
                end
            end
            state.edits.indent -= 1
            check_indent(x[3], state)
            state.offset += x[3].fullspan
        end

    elseif x.args === nothing
        state.offset += x.fullspan
    else
        if length(x) > 0
            for a in x
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
