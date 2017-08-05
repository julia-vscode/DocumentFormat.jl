struct FormatConfigParserException <: Exception
    file::String
    line::Int
    msg::String
end

FormatConfigParserException(file::String, line::Int, msgs...) =
    FormatConfigParserException(file, line, string(msgs...))

function Base.show(io::IO, err::FormatConfigParserException)
    print(io, "FormatConfig: parser error in $(err.file) on line $(err.line), ", (err.msg))
end

const WHITESPACE = [' ', '\t', '\n']

Base.@kwdef struct FormatConfig
    # Indents
    IndentWidth::Int = 4
    UseTab::Bool = false
    TabWidth::Int = 4
    IndentEXPR::Union = Union{cst.Begin,cst.Quote,cst.For,cst.While,cst.FunctionDef,cst.Macro,cst.Struct,cst.Let,cst.Try,cst.If}
    # Misc
    StripLineEnds::Bool = true
    NewLineEOF::Bool = true
    AlignAfterOpenBracket::Options.AlignAfterOpenBracket = Options.Align
end

const __config_settings_docstr = let
    default_config = FormatConfig()
    io = IOBuffer()
    sortperm_fields = sortperm(fieldnames(FormatConfig))
    for i in sortperm_fields
        setting, typ = fieldnames(FormatConfig)[i], FormatConfig.types[i]
        default_value = getfield(default_config, setting)
        print(io, " * `", setting, "` : ")
        if supertype(typ) <: Enum
            instances(typ)
            f(x) = (x == default_value ? "\e[1m" * string(x) * "\e[22m" : string(x))
            println(io, join("`" .* sort(collect(f.(instances(typ)))) .* "`", ", "))
        else
            println(io, "`", default_value, "`")
        end
    end
    String(take!(io))
end

"""
    parse_format_settings(file::AbstractString)

Parses `file` and returns an instance of `FormatConfig`.
Each line should consist of a setting with a value
formatted as `setting = value`. Comments work as in Julia
except that multiline comments are not supported.

Example of a setting file:
```
# JuliaCorpConfig
IndentWidth = 3
UseTab = true
TabWidth = 4
AlignAfterOpenBracket = Align
```

All the available settings and corresponding possible values (default value in bold):

$(__config_settings_docstr)
"""
function parse_format_settings(file::AbstractString)
    settingtype = Dict(k => v for (k,v) in zip(fieldnames(FormatConfig), FormatConfig.types))
    settingoption = Dict()
    fmterr(line, str...) = throw(FormatConfigParserException(file, line, str...))
    open(file, "r") do f
        for (current_line, line) in enumerate(eachline(f))
            line = split(line, '#')[1] # Remove comments
            line = replace(line, WHITESPACE, "")
            isempty(line) && continue
            splitted = split(line, '=')
            if length(splitted) != 2
                fmterr(current_line, "expected line to be formatted as `key = value`, read:\n    $line")
            end
            setting, value = Symbol(splitted[1]), splitted[2]
            if !haskey(settingtype, setting)
                fmterr(current_line, "unknown setting \"$setting\"")
            end
            T = settingtype[setting]
            if supertype(T) <: Enum
                i = findfirst(string.(instances(T)), value)
                if i == 0
                    fmterr(current_line, "unknown value \"$value\" for setting \"$(T.name.name)\", ",
                                         "possible values are ", join("\"" .* sort(collect(string.(instances(T)))) .* "\"", ", "))
                end
                option = instances(T)[i]
            else
                v = tryparse(T, value)
                if isnull(v)
                    fmterr(current_line, "failed to parse \"$value\" as a `$T`")
                end
                option = get(v)
            end
            settingoption[setting] = option
        end
    end
    FormatConfig(;settingoption...)
end

