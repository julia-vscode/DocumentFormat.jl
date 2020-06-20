function operator_pass(x, state)
    if typof(x) === CSTParser.BinaryOpCall
        if (CSTParser.precedence(x.args[2]) in (8, 13, 14, 16) && x.args[2].kind !== CSTParser.Tokens.ANON_FUNC) || x.args[2].fullspan == 0
            ensure_no_space_after(x.args[1], state, state.offset)
            ensure_no_space_after(x.args[2], state, state.offset + x.args[1].fullspan)
        else
            ensure_single_space_after(x.args[1], state, state.offset)
            ensure_single_space_after(x.args[2], state, state.offset + x.args[1].fullspan)
        end
    elseif typof(x) === CSTParser.WhereOpCall
        ensure_single_space_after(x.args[2], state, state.offset + x.args[1].fullspan)
        n = length(x.args)
        offset = state.offset + x.args[1].fullspan + x.args[2].fullspan
        for i = 3:n
            a = x.args[i]
            if i != n
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    elseif typof(x) === CSTParser.ColonOpCall
        offset = state.offset
        n = length(x.args)
        for (i, a) in enumerate(x.args)
            if i != n
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    elseif typof(x) === CSTParser.ChainOpCall || typof(x) == CSTParser.Comparison
        offset = state.offset
        n = length(x.args)
        for (i, a) in enumerate(x.args)
            if i != n
                ensure_single_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    end
end

function tuple_pass(x, state)
    if typof(x) === CSTParser.TupleH
        offset = state.offset
        n = length(x)
        for (i, a) in enumerate(x)
            i == n && continue
            if typof(a) === CSTParser.PUNCTUATION && kindof(a) === Tokens.COMMA && !(typof(x.args[i + 1]) === CSTParser.PUNCTUATION)
                ensure_single_space_after(a, state, offset)
            elseif !(typof(x.args[i + 1]) === CSTParser.Parameters)
                ensure_no_space_after(a, state, offset)
            end
            offset += a.fullspan
        end
    end
end

function curly_pass(x, state)
    if typof(x) === CSTParser.Curly
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
    if typof(x) === CSTParser.Call
        if issameline(state.offset, state.offset + x.span, state.lines)
            offset = state.offset + x.args[1].fullspan
            n = length(x)
            for (i, a) in enumerate(x)
                i == 1 && continue
                if typof(a) === CSTParser.PUNCTUATION && kindof(a) === Tokens.COMMA
                    ensure_single_space_after(a, state, offset)
                elseif i != n && !(typof(x.args[i + 1]) === CSTParser.Parameters)
                    ensure_no_space_after(a, state, offset)
                end
                offset += a.fullspan
            end
        else
            # space holder for splitting calls across lines
        end
    elseif typof(x) === CSTParser.Kw
        if state.opts.kwarg === "none"
            ensure_no_space_after(x.args[1], state, state.offset)
            ensure_no_space_after(x.args[2], state, state.offset + x.args[1].fullspan)
        elseif state.opts.kwarg === "single"
            ensure_exactly_single_space_after(x.args[1], state, state.offset)
            ensure_exactly_single_space_after(x.args[2], state, state.offset + x.args[1].fullspan)
        end
    end
end

function forloop_pass(x, state)
    if typof(x) === CSTParser.For
        offset = state.offset + x.args[1].fullspan
        for a in x.args[2]
            # convert iter = I into iter in I
            if typof(a) === CSTParser.BinaryOpCall && CSTParser.is_eq(a.args[2])
                offset += a.args[1].fullspan
                push!(state.edits, Edit(offset + 1:offset + 2, "in "))
                offset += a.args[2].fullspan
                offset += a.args[3].fullspan
            else
                offset += a.fullspan
            end
        end
    end
end

# TODO: move this to CSTParser?
function str_value(x)
    if typof(x) === CSTParser.PUNCTUATION
        kindof(x) == Tokens.LPAREN && return "("
        kindof(x) == Tokens.LBRACE && return "{"
        kindof(x) == Tokens.LSQUARE && return "["
        kindof(x) == Tokens.RPAREN && return ")"
        kindof(x) == Tokens.RBRACE && return "}"
        kindof(x) == Tokens.RSQUARE && return "]"
        kindof(x) == Tokens.COMMA && return ","
        kindof(x) == Tokens.SEMICOLON && return ";"
        kindof(x) == Tokens.AT_SIGN && return "@"
        kindof(x) == Tokens.DOT && return "."
        return ""
    elseif typof(x) === CSTParser.IDENTIFIER || typof(x) === CSTParser.LITERAL || typof(x) === CSTParser.OPERATOR || typof(x) === CSTParser.KEYWORD
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
    if typof(x) === CSTParser.MacroCall && typof(x.args[1]) === CSTParser.GlobalRefDoc
        # Align global docstring to:
        #
        # """
        # doc
        # """
        #
        # If the doc is single quoted i.e. "doc", they will be replaced with triple quotes.
        offset = state.offset + x.args[1].fullspan
        doc = x.args[2]

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
    if typof(x) === CSTParser.KEYWORD &&
        kindof(x) in (CSTParser.Tokens.ABSTRACT,
                      CSTParser.Tokens.BAREMODULE,
                      CSTParser.Tokens.CONST,
                      CSTParser.Tokens.DO,
                      CSTParser.Tokens.ELSEIF,
                      CSTParser.Tokens.EXPORT,
                      CSTParser.Tokens.FOR,
                      CSTParser.Tokens.FUNCTION,
                      CSTParser.Tokens.GLOBAL,
                      CSTParser.Tokens.IF,
                      CSTParser.Tokens.IMPORT,
                      CSTParser.Tokens.LOCAL,
                      CSTParser.Tokens.MACRO,
                      CSTParser.Tokens.MODULE,
                      CSTParser.Tokens.MUTABLE,
                      CSTParser.Tokens.OUTER,
                      CSTParser.Tokens.PRIMITIVE,
                      CSTParser.Tokens.STRUCT,
                      CSTParser.Tokens.TYPE,
                      CSTParser.Tokens.USING,
                      CSTParser.Tokens.WHILE)
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
                !(y.typ == CSTParser.LITERAL && y.kind in (CSTParser.Tokens.STRING, CSTParser.Tokens.TRIPLE_STRING, CSTParser.Tokens.CMD, CSTParser.Tokens.TRIPLE_CMD)) : true)
                push!(state.edits, Edit((n - i2) + 1:(n - i1), ""))
            end
        end
    end
end
