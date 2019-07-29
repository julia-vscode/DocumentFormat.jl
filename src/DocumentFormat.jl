module DocumentFormat
using CSTParser
using CSTParser.Tokenize
import CSTParser.Tokenize.Tokens

mutable struct FormatOptions
    indent::Int
    indents::Bool
    ops::Bool
    tuples::Bool
    curly::Bool
    calls::Bool
    iterOps::Bool
    comments::Bool
    docs::Bool
    lineends::Bool
end
FormatOptions() = FormatOptions(4, true, true, true, true, true, true, true, true, true)

struct Edit{T}
    loc::T
    text::String
end

mutable struct State{T}
    offset::Int
    edits::T
    opts::FormatOptions
end

function format(text, formatopts::FormatOptions = FormatOptions())
    original_ast = CSTParser.remlineinfo!(Meta.parse(string("begin\n", text, "\nend")))
    state = State(0, Edit[], formatopts)
    x = CSTParser.parse(text, true)
    if formatopts.ops
        pass(x, state, operator_pass)
    end
    if formatopts.tuples
        state.offset = 0
        pass(x, state, tuple_pass)
    end
    if formatopts.curly
        state.offset = 0
        pass(x, state, curly_pass)
    end
    if formatopts.calls
        state.offset = 0
        pass(x, state, call_pass)
    end
    if formatopts.iterOps
        state.offset = 0
        pass(x, state, forloop_pass)
    end
    if formatopts.comments
        comments_pass(text, state)
    end
    if formatopts.docs
        state.offset = 0
        pass(x, state, doc_pass)
    end
    if formatopts.lineends
        lineends_pass(text, state)
    end
    sort!(state.edits, lt = (a, b)->first(a.loc) < first(b.loc), rev = true)

    for i = 1:length(state.edits)
        text = apply(text, state.edits[i])
    end
    if formatopts.indents
        text = indents(text, state.opts)
    end
    new_ast = CSTParser.remlineinfo!(Meta.parse(string("begin\n", text, "\nend")))
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

function ensure_single_space_after(x, state, offset)
    if x.fullspan == x.span
        push!(state.edits, Edit(offset + x.fullspan, " "))

    end
end

function ensure_no_space_after(x, state, offset)
    if x.fullspan != x.span
        push!(state.edits, Edit(offset .+ (x.span + 1:x.fullspan), ""))
    end
end

function apply(text, edit::Edit{Int})
    v = Vector{UInt8}(deepcopy(text))
    String(vcat(v[1:edit.loc], Vector{UInt8}(edit.text), v[edit.loc + 1:end]))
end
function apply(text, edit::Edit{UnitRange{Int}})
    v = Vector{UInt8}(deepcopy(text))
    String(vcat(v[1:first(edit.loc) - 1], Vector{UInt8}(edit.text), v[last(edit.loc) + 1:end]))
end
include("passes.jl")
include("indents.jl")
end
