const INDENT = 4

mutable struct IndentState
    indent::Int
    lines
    edits
end

function get_lines(text)
    lines = Tuple{Int,Int}[]
    pt = Tokens.EMPTY_TOKEN(Tokens.Token)
    for t in CSTParser.Tokenize.tokenize(text)
        if pt.endpos[1] != t.startpos[1]
            if t.kind == Tokens.WHITESPACE
                nl = findfirst("\n", t.val) != nothing
                if !nl
                    push!(lines, (length(t.val), 0))
                else
                end
            else
                push!(lines, (0, 0))
            end
        elseif t.startpos[1] != t.endpos[1] && t.kind == Tokens.TRIPLE_STRING
            nls = findall(x -> x == '\n', t.val)
            for nl in nls
                push!(lines, (t.startpos[2]-1, nl + t.startbyte))
            end
        elseif t.startpos[1] != t.endpos[1] && t.kind == Tokens.WHITESPACE
            push!(lines, (t.endpos[2], t.endbyte - t.endpos[2] + 1))
        end
        pt = t
    end
    lines
end

function indent_pass(x, state)
    if x isa CSTParser.EXPR{CSTParser.FileH}
        for a in x
            check_indent(a, state)
            indent_pass(a, state)
        end
    elseif x isa CSTParser.EXPR{CSTParser.Begin} || (x isa CSTParser.EXPR{CSTParser.Quote} && x.args[1] isa CSTParser.KEYWORD && x.args[1].kind == Tokens.QUOTE)
        state.offset += x.args[1].fullspan
        state.edits.indent += 1
        for a in x.args[2]
            check_indent(a, state)
            indent_pass(a, state)
        end
        state.edits.indent -= 1
        check_indent(x.args[3], state)
        state.offset += x.args[3].fullspan
    elseif x isa CSTParser.EXPR{T} where T <: Union{CSTParser.FunctionDef,CSTParser.Macro,CSTParser.For,CSTParser.While,CSTParser.Let,CSTParser.Struct}
        state.offset += x.args[1].fullspan + x.args[2].fullspan
        if x.args[3] isa CSTParser.EXPR{CSTParser.Block}
            state.edits.indent += 1
            for a in x.args[3]
                check_indent(a, state)
                indent_pass(a, state)
            end
            state.edits.indent -= 1
            check_indent(x.args[4], state)
            state.offset += x.args[4].fullspan
        else
            check_indent(x.args[3], state)
            state.offset += x.args[3].fullspan
        end
    elseif x isa CSTParser.EXPR{CSTParser.MacroCall}
        if x.args[1] isa CSTParser.EXPR{CSTParser.GlobalRefDoc}
            state.offset += x.args[1].fullspan

            doc = x.args[2]
            doc_strs = split(CSTParser.str_value(doc), "\n")

            state.offset += 4
            for s in doc_strs
                l = length(s)
                a = CSTParser.LITERAL(l+1, 1:l, s, Tokens.STRING)
                check_indent(a, state)
                indent_pass(a, state)
            end
            state.offset += 3

            check_indent(x.args[3], state)
            indent_pass(x.args[3], state)
        else
            for a in x
                indent_pass(a, state)
            end
        end
    elseif x isa CSTParser.EXPR{CSTParser.Mutable}
        state.offset += x.args[1].fullspan + x.args[2].fullspan + x.args[3].fullspan
        if x.args[4] isa CSTParser.EXPR{CSTParser.Block}
            state.edits.indent += 1
            for a in x.args[4]
                check_indent(a, state)
                indent_pass(a, state)
            end
            state.edits.indent -= 1
            check_indent(x.args[5], state)
            state.offset += x.args[5].fullspan
        else
            check_indent(x.args[3], state)
            state.offset += x.args[4].fullspan
        end
    elseif x isa CSTParser.EXPR{CSTParser.Try}
        state.offset += x.args[1].fullspan

        state.edits.indent += 1
        for a in x.args[2]
            check_indent(a, state)
            indent_pass(a, state)
        end
        state.edits.indent -= 1
        check_indent(x.args[3], state)
        state.offset += x.args[3].fullspan + x.args[4].fullspan

        state.edits.indent += 1
        for a in x.args[5]
            check_indent(a, state)
            indent_pass(a, state)
        end
        state.edits.indent -= 1
        check_indent(x.args[6], state)
        state.offset += x.args[6].fullspan

    elseif x isa CSTParser.EXPR{CSTParser.If}
        if first(x.args) isa CSTParser.KEYWORD && first(x.args).kind == Tokens.IF
            state.offset += x.args[1].fullspan + x.args[2].fullspan
            state.edits.indent += 1
            for a in x.args[3]
                check_indent(a, state)
                indent_pass(a, state)
            end
            state.edits.indent -= 1
            check_indent(x.args[4], state)
            state.offset += x.args[4].fullspan
            if length(x.args) > 4
                state.edits.indent += 1
                for a in x.args[5]
                    check_indent(a, state)
                    indent_pass(a, state)
                end
                state.edits.indent -= 1
                check_indent(x.args[6], state)
                state.offset += x.args[6].fullspan
            end
        else
            state.offset += x.args[1].fullspan
            for a in x.args[2]
                check_indent(a, state)
                indent_pass(a, state)
            end
            if length(x.args) > 2
                state.edits.indent -= 1
                check_indent(x.args[3], state)
                state.offset += x.args[3].fullspan
                state.edits.indent += 1
                for a in x.args[4]
                    check_indent(a, state)
                    indent_pass(a, state)
                end

            end
        end

    elseif x isa CSTParser.LeafNode
        state.offset += x.fullspan
    else
        for a in x
            indent_pass(a, state)
        end
    end
    state
end

function check_indent(x, state)
    for (i,l) in state.edits.lines
        if state.offset == l+i
            if state.edits.indent*INDENT != i
                #= @info CSTParser.str_value(CSTParser.get_name(x)), state.edits.indent*INDENT, i, state.offset =#
                push!(state.edits.edits, (l, state.edits.indent*INDENT - i))
            end
        end
    end
end

function indents(text)
    x = CSTParser.parse(text, true)
    lines = get_lines(text)
    state = indent_pass(x, State(0, IndentState(0, lines, [])))

    sort!(state.edits.edits, lt = (a,b) -> a[1] < b[1], rev = true)
    for (l, d) in state.edits.edits
        if d > 0
            text = string(text[1:l], " "^d, text[l+1:end])
        else
            text = string(text[1:l], text[l+1-d:end])
        end
    end
    return text
end
