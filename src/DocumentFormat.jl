module DocumentFormat
using CSTParser
import Tokenize.Tokens
import CSTParser: EXPR, OPERATOR, IDENTIFIER, PUNCTUATION
import CSTParser: UnaryOpCall, UnarySyntaxOpCall, BinaryOpCall, BinarySyntaxOpCall
import CSTParser: trailing_ws_length, precedence

export format, formatpkg
const cst = CSTParser

include("options.jl")
include("formatconfig.jl")

struct TextEdit
    range::UnitRange{Int}
    text::String
end
Deletion(range::UnitRange{Int}) = TextEdit(range, "")

mutable struct Diagnostic
    message::String
    edits::Vector{TextEdit} 
end

mutable struct FormatState
    content::String
    offset::Int
    diagnostics::Vector{Diagnostic}
    line_ranges::Vector{UnitRange{Int}}
    indent::Int
    config::FormatConfig
end

FormatState(str, config) = FormatState(str, 0, [], get_line_ranges(str), 0, config)
FormatState(str) = FormatState(str, FormatConfig())

include("operators.jl")
include("indents.jl")
include("utils.jl")

function format(str::String, config = FormatConfig())
    x = CSTParser.parse(str, true)
    F = FormatState(str, config)
    format(x, F)
    F.config.StripLineEnds && strip_empty_line_ends(F)
    F.config.NewLineEOF && end_file_newline(F)
    apply(str, F)
end

"""
    formatpkg(pkg::String; kwargs...)

Format the package `pkg` either given as the name of the package
or as a full path to the package.

**Keywords**:
* `config::String` - Path to the config file to use. If not set, will look for a
  `.julia-config` in the package directory and if not found will use the default
  config
* `force::Bool` - Format files even if the package repository is in a dirty state, defaults to `false`.
"""
function formatpkg(pkg::String; configpath=nothing, force=false)
    # TODO: Better exceptions
    # Figure out path to package
    path = if isdir(Pkg.dir(pkg))
        Pkg.dir(pkg)
    elseif isdir(pkg)
        pkg
    else
        error("could not find the package $pkg")
    end
 
    # Check dirtiness of repo
    gitrepo =
        try
            LibGit2.GitRepo(path)
        catch
            nothing
        end
    if gitrepo !== nothing
        if LibGit2.isdirty(gitrepo) && !force
            error("package repository at $path is dirty and `force = false`")
        end
    end

    # Get the configuration
    config = if configpath == nothing
        pkgconfigpath = joinpath(path, ".julia-config")
        if isfile(pkgconfigpath)
            parse_format_settings(pkgconfigpath)
        else
            FormatConfig()
        end
    else
        parse_format_settings(configpath)
    end

    # if f returns something, an error happened in f
    function apply_to_julia_files_in_folder(f)
        for (root, dirs, files) in walkdir(path)
            for file in files
                if endswith(file, ".jl")    
                    jlpath = joinpath(root, file)
                    err = f(jlpath)
                    if err != nothing
                        return err, jlpath
                    end
                end
            end
        end
        return nothing, nothing
    end

    # Make sure we can parse each file in the package before doing any formatting
    # TODO: Perhaps this is annoying if a user has scratch files in the repo, use .gitignore?
    err, path_to_file_with_error = apply_to_julia_files_in_folder(
        function(file)
            try     
                CSTParser.parse(readstring(file))
            catch er
                return er
            end
            return nothing
        end
    )
    if err != nothing
        error("error when parsing $path_to_file_with_error, no formatting done, error: \n",
              sprint(Base.showerror, err))
    end

    # Ok, all files parsed correctly, let's do the formatting
    err, path_to_file_with_error = apply_to_julia_files_in_folder(
        function(file)
            oldstr = readstring(file)
            newstr = format(oldstr, config)
            # Check that newstr parses TODO: Should this be an option to `format`?
            try
                CSTParser.parse(newstr)
            catch e
                error("internal error: the formatted file failed to parse, error: \n",
                      sprint(Base.showerror, e))
            end
            open(file, "w") do f
                print(f, newstr)
            end
        end
    )

    # All files are formatted, we are happy
    return 
end
    

function format(x::EXPR{T}, F::FormatState) where T
    if T <: IndentEXPR
        indent(F)
    end
    for a in x.args
        offset = F.offset
        format(a, F)
        F.offset = offset + a.fullspan
    end
    if T <: IndentEXPR
        deindent(F)
    end
end

function format(x::EXPR{cst.TupleH}, F::FormatState)
    nargs = length(x.args)
    hasbrackets = first(x.args) isa EXPR{PUNCTUATION{Tokens.LPAREN}}
    for (i, a) in enumerate(x.args)
        offset = F.offset
        if a isa EXPR{PUNCTUATION{Tokens.COMMA}} && !(hasbrackets && i == nargs  - 1)
            i < nargs && trailing_ws(a, F)
        else
            i < nargs && no_trailing_ws(a, F)
            format(a, F)
        end
        F.offset = offset + a.fullspan
    end
end

function format(x::EXPR{cst.Curly}, F::FormatState)
    nargs = length(x.args)
    for (i, a) in enumerate(x.args)
        offset = F.offset
        if a isa EXPR{PUNCTUATION{Tokens.LPAREN}} || (i == nargs - 1 && !(x.args[i] isa EXPR{PUNCTUATION{Tokens.COMMA}}))
            no_trailing_ws(a, F)
        elseif i < nargs 
            no_trailing_ws(a, F)
            format(a, F)
        end
        F.offset = offset + a.fullspan
    end
end

function format(x::EXPR{cst.Call}, F::FormatState)
    nargs = length(x.args)
    for (i, a) in enumerate(x.args)
        offset = F.offset
        if a isa EXPR{PUNCTUATION{Tokens.COMMA}} 
            trailing_ws(a, F)
        else
            i < nargs && !(x.args[i+1] isa EXPR{cst.Parameters}) && no_trailing_ws(a, F)
            format(a, F)
        end
        F.offset = offset + a.fullspan
    end
end

end
