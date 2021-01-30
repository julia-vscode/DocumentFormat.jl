function operator_pass(x, state)
    if CSTParser.isbinarycall(x) || (CSTParser.isbinarysyntax(x) && headof(x).fullspan > 0)
        opprec = CSTParser.get_prec(CSTParser.isdotted(x[2]) ? valof(x[2])[2:end] : valof(x[2]))
        if (opprec in (CSTParser.ColonOp, CSTParser.PowerOp, CSTParser.DeclarationOp, CSTParser.DotOp) && valof(x[2]) != "->") || x[2].fullspan == 0
            ensure_no_space_after(x[1], state, state.offset)
            ensure_no_space_after(x[2], state, state.offset + x[1].fullspan)
        else
            ensure_single_space_after(x[1], state, state.offset)
            ensure_single_space_after(x[2], state, state.offset + x[1].fullspan)
        end
    elseif CSTParser.iswhere(x)
        ensure_single_space_after(x[2], state, state.offset + x[1].fullspan)
        n = length(x)
        offset = state.offset + x[1].fullspan + x[2].fullspan
        for i = 3:n
            a = x[i]
            if i != n
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    elseif CSTParser.iscall(x) && CSTParser.isoperator(x[2]) && valof(x[2]) == ":"
        offset = state.offset
        n = length(x)
        for (i, a) in enumerate(x)
            if i != n
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    elseif (headof(x) === :call && valof(x.args[1]) in ("+", "*") && length(x) > 3) || headof(x) == :comparison
        offset = state.offset
        n = length(x)
        for (i, a) in enumerate(x)
            if i != n
                ensure_single_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    end
end

function tuple_pass(x, state)
    if headof(x) === :tuple
        offset = state.offset
        n = length(x)
        for (i, a) in enumerate(x)
            i == n && continue
            wsrange = nextind(state.text, offset + a.span):offset + a.fullspan
            ws = state.text[wsrange]
            if all(in((' ', '\t')), ws)
                if headof(a) === :COMMA && !(CSTParser.ispunctuation(x[i + 1]))
                    ensure_single_space_after(a, state, offset)
                elseif !(headof(x[i + 1]) === :parameters)
                    ensure_no_space_after(a, state, offset)
                end
            end
            offset += a.fullspan
        end
    end
end

function curly_pass(x, state)
    if headof(x) === :curly
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
    if headof(x) === :call && !(CSTParser.isbinarycall(x) || CSTParser.ischainedcall(x) || (CSTParser.isoperator(x.args[1]) && valof(x.args[1]) == ":"))
        CSTParser.iscolon
        if issameline(state.offset, state.offset + x.span, state.lines)
            offset = state.offset + x[1].fullspan
            n = length(x)
            for (i, a) in enumerate(x)
                i == 1 && continue
                if headof(a) === :COMMA
                    ensure_single_space_after(a, state, offset)
                elseif i != n && !(headof(x[i + 1]) === :parameters)
                    ensure_no_space_after(a, state, offset)
                end
                offset += a.fullspan
            end
        else
            # space holder for splitting calls across lines
        end
    elseif headof(x) === :kw
        if state.opts.kwarg === "none"
            ensure_no_space_after(x[1], state, state.offset)
            ensure_no_space_after(x[2], state, state.offset + x[1].fullspan)
        elseif state.opts.kwarg === "single"
            ensure_exactly_single_space_after(x[1], state, state.offset)
            ensure_exactly_single_space_after(x[2], state, state.offset + x[1].fullspan)
        end
    end
end

function forloop_pass(x, state)
    if headof(x) === :for
        offset = state.offset + x[1].fullspan
        for a in x[2]
            # convert iter = I into iter in I
            if CSTParser.isassignment(a) && (a.trivia === nothing || isempty(a.trivia))
                offset += a[1].fullspan
                push!(state.edits, Edit(offset + 1:offset + 2, "in "))
                offset += a[2].fullspan
                offset += a[3].fullspan
            else
                offset += a.fullspan
            end
        end
    end
end

# TODO: move this to CSTParser?
function str_value(x)
    if CSTParser.ispunctuation(x)
        headof(x) === :LPAREN && return "("
        headof(x) === :LBRACE && return "{"
        headof(x) === :LSQUARE && return "["
        headof(x) === :RPAREN && return ")"
        headof(x) === :RBRACE && return "}"
        headof(x) === :RSQUARE && return "]"
        headof(x) === :COMMA && return ","
        headof(x) === :SEMICOLON && return ";"
        headof(x) === :AT_SIGN && return "@"
        headof(x) === :DOT && return "."
        return ""
    elseif headof(x) === :IDENTIFIER || CSTParser.isliteral(x) || CSTParser.isoperator(x) || CSTParser.iskeyword(x)
        return CSTParser.str_value(x)
    else
        s = ""
        for a in x
            s *= str_value(a)
        end
        return s
    end
end


function doc_pass(x, state)
    return
    if headof(x) === :macrocall && headof(x[1]) === :globalrefdoc
        # Align global docstring to:
        #
        # """
        # doc
        # """
        #
        # If the doc is single quoted i.e. "doc", they will be replaced with triple quotes.
        offset = state.offset + x[1].fullspan
        doc = x[2]

        val = str_value(doc)

        # s = escape_string(strip(val, ['\n']), "\$")
        s = strip(val, ['\n'])
        ds = string("\"\"\"\n", s, "\n", "\"\"\"\n")

        # Check if docstring needs to be edited
        if length(ds) != doc.fullspan || s != val
            # Remove previous docstring
            push!(state.edits, Edit(offset + 1:offset + doc.fullspan, ""))
            # Append newly formatted docstring
            push!(state.edits, Edit(offset, ds))
        end
    end
end

function kw_pass(x, state)
    if CSTParser.iskeyword(x) &&
        headof(x) in (:ABSTRACT,
                      :BAREMODULE,
                      :CONST,
                      :ELSEIF,
                      :EXPORT,
                      :FOR,
                      :FUNCTION,
                      :GLOBAL,
                      :IF,
                      :IMPORT,
                      :LOCAL,
                      :MACRO,
                      :MODULE,
                      :MUTABLE,
                      :OUTER,
                      :PRIMITIVE,
                      :STRUCT,
                      :TYPE,
                      :USING,
                      :WHILE) || (headof(x) === :DO && x.args !== nothing && length(x.args) > 1 && x.args[2].args !== nothing && x.args[2].args[1].fullspan > 0)
        ensure_exactly_single_space_after(x, state, state.offset)
    end
end



function comments_pass(text, state)
    ts = tokenize(text)
    while !Tokenize.Lexers.eof(ts)
        t = Tokenize.Lexers.next_token(ts)
        if Tokens.kind(t) == Tokens.COMMENT
            val = Vector{UInt8}(t.val)
            if length(val) > 1 && val[2] == 0x3d # multiline
                if !(val[3] in (0x20, 0x09)) # ensure single space at start
                    push!(state.edits, Edit(t.startbyte + 2, " "))
                end
                if length(val) > 5 # ensure single space at
                    for i = length(val) - 2:-1:3
                        if val[i] in (0x20, 0x09, 0x0a, 0x0d)
                            continue
                        else
                            push!(state.edits, Edit(t.startbyte .+ (i + 1:length(val) - 2), " "))
                            break
                        end
                    end
                end

            elseif length(val) > 1
                t.startpos == (1, 1) && val[2] == 0x21 && continue # don't mess with the shebang
                if !(val[2] in (0x20, 0x09, 0x23)) # ensure single space at start
                    push!(state.edits, Edit(t.startbyte + 1, " "))
                end
            end
        end
    end
end



function lineends_pass(text, x, state)
    n = lastindex(text)
    io = IOBuffer(reverse(text))
    while !eof(io)
        c = read(io, Char)
        if c === '\n' && !eof(io)
            Base.peek(io) == 0x0d && read(io, Char) # crlf
            i1 = i2 = position(io)
            pc = Base.peek(io)
            while !eof(io) && pc in (0x20, 0x09)
                i2 = position(io)
                pc = read(io, UInt8)
            end
            if i1 != i2 && (y = get_expr(x, n - i1); y isa CSTParser.EXPR ?
                !(headof(y) in (:STRING, :TRIPLE_STRING, :CMD, :TRIPLE_CMD)) : true)
                push!(state.edits, Edit((n - i2) + 1:(n - i1), ""))
            end
        end
    end
end
