module DocumentFormat
using CSTParser
using CSTParser.Tokenize
import CSTParser.Tokenize.Tokens
using CSTParser: typof, kindof, EXPR
using FilePathsBase

const default_options = (4, true, true, true, true, true, true, true, true, false, true, "none")

struct FormatOptions
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
    keywords::Bool
    kwarg::String # Options arg-> "none", "single", "off"
end
FormatOptions() = FormatOptions(default_options...)

# Provide sane default constructor for applications who don't know option specifics.
# Any nothing argument is given a sane default value.
FormatOptions(options::Vararg{Union{Int,Bool,Nothing},length(default_options)}) =
    FormatOptions(something.(options, default_options)...)

struct Edit{T}
    loc::T
    text::String
end

mutable struct State{T}
    offset::Int
    edits::T
    opts::FormatOptions
    text
    lines::Vector{Tuple{Int,Int}}
end

function format(original_text::AbstractString, formatopts::FormatOptions=FormatOptions())
    text = deepcopy(original_text)
    original_ast = CSTParser.remlineinfo!(Meta.parse(string("begin\n", text, "\nend"), raise=false))
    if original_ast.head == :error
        @warn ("There was an error in the original ast, original text returned.")
        return text
    end
    state = State(0, Edit[], formatopts, text, get_lines(text))
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
    if formatopts.keywords
        state.offset = 0
        pass(x, state, kw_pass)
    end

    if formatopts.lineends
        lineends_pass(text, x, state)
    end
    sort!(state.edits, lt=(a, b) -> first(a.loc) < first(b.loc), rev=true)

    for i = 1:length(state.edits)
        text = apply(text, state.edits[i])
    end
    if formatopts.indents
        text = indents(text, state.opts)
    end
    new_ast = CSTParser.remlineinfo!(Meta.parse(string("begin\n", text, "\nend"), raise=false))
    if new_ast.head == :error
        @warn ("There was an error in the formatted text, original returned.")
        return original_text
    elseif original_ast != new_ast
        @warn ("Mismatch between AST of original and formatted text")
        return original_text
    end

    return text
end

function format(path::AbstractPath, formatopts::FormatOptions=FormatOptions())
    if isfile(path)
        extension(path) != "jl" && error("Only .jl files can be formatted.")

        content = read(path, String)
        formatted_content = format(content, formatopts)
        write(path, formatted_content)
    elseif exists(path)
        for p in walkpath(path)
            if extension(p) == "jl"
                format(p, formatopts)
            end
        end
    else
        error("Invalid path.")
    end

    return nothing
end

function isformatted(text::AbstractString, formatopts::FormatOptions=FormatOptions())
    original_text = text
    new_text = format(text, formatopts)

    return original_text == new_text
end

function isformatted(path::AbstractPath, formatopts::FormatOptions=FormatOptions())
    if isfile(path)
        extension(path) != "jl" && error("Only .jl files can be formatted.")

        content = read(path, String)
        formatted_content = format(content, formatopts)

        return content == formatted_content
    elseif exists(path)
        for p in walkpath(path)
            if extension(p) == "jl"
                if !isformatted(p, formatopts)
                    return false
                end
            end
        end

        return true
    else
        error("Invalid path.")
    end

    return nothing
end

function pass(x, state, f=(x, state) -> nothing)
    f(x, state)
    if x.args isa Vector{EXPR}
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

function ensure_exactly_single_space_after(x, state, offset)
    if x.fullspan !== x.span + 1
        push!(state.edits, Edit(offset .+ (x.span + 1:x.fullspan), " "))
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
include("utils.jl")
end
