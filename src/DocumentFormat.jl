module DocumentFormat
using CSTParser
import CSTParser.Tokenize.Tokens

mutable struct FormatOptions
    indent::Int
end
FormatOptions() = FormatOptions(4)

struct Edit{T}
    loc::T
    text::String
end

mutable struct State{T}
    offset::Int
    edits::T
    opts::FormatOptions
end

function format(text; convert_iterator_ops = false)
    original_ast = CSTParser.remlineinfo!(Meta.parse(string("begin\n",text, "\nend")))
    state = State(0, Edit[], FormatOptions())
    x = CSTParser.parse(text, true)
    pass(x, state, operator_pass)
    state.offset = 0
    pass(x, state, tuple_pass)
    state.offset = 0
    pass(x, state, curly_pass)
    state.offset = 0
    pass(x, state, call_pass)
    if convert_iterator_ops
        state.offset = 0
        pass(x, state, forloop_pass)
    end
    state.offset = 0
    pass(x, state, doc_pass)
    sort!(state.edits, lt = (a, b)->first(a.loc) < first(b.loc), rev = true)

    for i = 1:length(state.edits)
        text = apply(text, state.edits[i])
    end
    text = indents(text, state.opts)
    new_ast = CSTParser.remlineinfo!(Meta.parse(string("begin\n",text, "\nend")))
    original_ast != new_ast && error("Mismatch between AST of original and formatted text")
    return text
end

function pass(x, state, f = (x, state)->nothing)
    f(x, state)
    if x.args isa Vector
        for a in x.args
            pass(a, state, f)
        end
    else
        state.offset += x.fullspan
    end
    state
end

function get_last_char_index(s)
    len = lastindex(s)
    pos = len > 5 ? len - 5 : 1
    try
        while true
            try; s[pos]; catch StringIndexError; pos += 1; continue; end
            break
        end
    catch BoundsError
        error("A UTF-8 char longer than 5 bytes was encountered, which is not supported currently.")
    end
    while true
        if (nextpos = nextind(s, pos)) > len ; break; end
        pos = nextpos
    end
    pos
end


function ensure_single_space_after(x, state, offset)
    if x.fullspan == x.span
        if x.typ === CSTParser.OPERATOR
            if x.span > 1 && length(String(Expr(x))) == 1
                push!(state.edits, Edit(offset + 1, " "))
            else
                push!(state.edits, Edit(offset + x.fullspan, " "))
            end
        elseif x.val isa String
            last_char_index = get_last_char_index(x.val)
            push!(state.edits, Edit(offset + last_char_index, " "))
        else
            push!(state.edits, Edit(offset + x.fullspan, " "))
        end
    end
end

function ensure_no_space_after(x, state, offset)
    if x.fullspan != x.span
        push!(state.edits, Edit(offset .+ (x.span + 1:x.fullspan), ""))
    end
end

function apply(text, edit::Edit{Int})
    string(text[1:edit.loc], edit.text, text[nextind(text, edit.loc):end])
end
function apply(text, edit::Edit{UnitRange{Int}})
    string(text[1:prevind(text, first(edit.loc))], edit.text, text[nextind(text, last(edit.loc)):end])
end
include("passes.jl")
include("indents.jl")
end
