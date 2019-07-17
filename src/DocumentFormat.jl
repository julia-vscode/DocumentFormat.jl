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
    String(vcat(v[1:first(edit.loc)-1], Vector{UInt8}(edit.text), v[last(edit.loc) + 1: end]))
end
include("passes.jl")
include("indents.jl")
end
