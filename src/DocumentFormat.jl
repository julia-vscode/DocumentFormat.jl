module DocumentFormat
using CSTParser, Tokenize
import Tokenize.Tokens
import CSTParser: EXPR, OPERATOR, IDENTIFIER, PUNCTUATION, KEYWORD, LITERAL
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

include("utils.jl")

function format(str::String, config = FormatConfig())
    x, ps = CSTParser.parse(CSTParser.ParseState(str), true)
    
    if ps.errored
        return str
    end
    x0 = Expr(CSTParser.parse(string("begin\n", str, "\nend")))
    F = FormatState(str, config)
    format(x, F)
    F.offset = 0
    indent(x, F)
    F.config.StripLineEnds && strip_empty_line_ends(F)
    F.config.NewLineEOF && end_file_newline(F)
    str1 = apply(str, F)
    x1 = Expr(CSTParser.parse(string("begin\n", str1, "\nend")))
    if x0 == x1
        return str1
    else
        edits = TextEdit[]
        for d in F.diagnostics
            append!(edits, d.edits)
        end
        sort!(edits, by = x -> -first(x.range))
        str1 = str
        for e in edits
            str2 = string(str1[1:first(e.range)-1], e.text, str1[last(e.range)+1:end])
            x2 = Expr(CSTParser.parse(string("begin\n", str1, "\nend")))
            if x2 == x0
                str1 = str2
            end
        end
        str1
        return str
    end
end


"""
    formatpkg(pkg::String; kwargs...)

Format the package `pkg` either given as the name of the package
or as a full path to the package.

**Keywords**:
* `configpath::String` - Path to the config file to use. If not set, will look for a
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
                CSTParser.parse(read(file, String))
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
            oldstr = read(file, String)
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

function format(x, F::FormatState)
    ws, no_ws = get_ws_arg(x, F)
    for (i, a) in enumerate(x)
        ws(i) && trailing_ws(a, F)
        no_ws(i) && no_trailing_ws(a, F)
        format(a, F)
    end
end

function format(x::T, F::FormatState) where T <: Union{IDENTIFIER,PUNCTUATION,<:KEYWORD,LITERAL,OPERATOR}
    F.offset += x.fullspan
end

function get_ws_arg(x, F)
    if x isa EXPR{CSTParser.Call}
        nargs = length(x.args)
        has_params = x.args[end-1] isa EXPR{CSTParser.Parameters}
        ws = i -> i < length(x.args) - has_params - CSTParser.is_comma(x.args[nargs - 1 - has_params]) && i != 2 && iseven(i)
        no_ws = i -> i < length(x.args) && (isodd(i) || i == 2 || (CSTParser.is_comma(x.args[nargs - 1]) && i == nargs - 1)) && !(has_params && i == nargs - 2)
    elseif x isa EXPR{CSTParser.Parameters}
        nargs = length(x.args)
        ws = i -> i != nargs && iseven(i)
        no_ws = i -> i != nargs && isodd(i)
    elseif x isa EXPR{CSTParser.Kw}
        ws = i -> false
        no_ws = i -> i != 3 && F.config.KW_WS
    elseif x isa EXPR{CSTParser.TupleH} && !isempty(x.args)
        nargs = length(x.args)
        if CSTParser.is_lparen(x.args[1])
            ws = i -> i < length(x.args) -1  && i != 1 && isodd(i)
            no_ws = i -> i != length(x.args) && (iseven(i) || i == 1)
        else
            ws = i -> i != length(x.args) && iseven(i)
            no_ws = i -> i != length(x.args) && isodd(i)
        end
    elseif x isa EXPR{CSTParser.Curly}
        ws = i -> false
        no_ws = i -> i != length(x.args)
    elseif x isa CSTParser.UnaryOpCall || x isa CSTParser.UnarySyntaxOpCall
        ws = i -> false
        no_ws = i -> i == 1
    elseif (x isa CSTParser.BinaryOpCall || x isa CSTParser.BinarySyntaxOpCall) && length(x.op.span) > 0
        if nows(x.op, F.config.No_WS_OP_group, F.config.No_WS_OP_indv)
            ws = i -> false
            no_ws = i -> i != 3
        else
            ws = i -> i != 3
            no_ws = i -> false
        end
    elseif x isa CSTParser.ConditionalOpCall
        ws = i -> i != 5
        no_ws = i -> false
    elseif x isa EXPR{CSTParser.ColonOpCall}
        nargs = length(x.args)
        ws = i -> false
        no_ws = i -> i != 5
    elseif x isa EXPR{CSTParser.ChainOpCall} || x isa EXPR{CSTParser.Comparison}
        nargs = length(x.args)
        if nows(x.args[2], F.config.No_WS_OP_group, F.config.No_WS_OP_indv)
            ws = i -> false
            no_ws = i -> i != nargs
        else
            ws = i -> i != nargs
            no_ws = i -> false
        end
    else
        ws = i -> false
        no_ws = i -> false
    end
    ws, no_ws
end

function nows(x::CSTParser.OPERATOR, group, individual)
    P = precedence(x)
    (P in group || x.kind in individual) && x.kind != Tokenize.Tokens.ANON_FUNC
end

function indent_block(x, F, doindent)
    x.fullspan == 0 && return
    if doindent 
        F.indent += 1
    end
    for a in x
        doindent && check_indent(F)
        indent(a, F)
    end
    if doindent
        F.indent -= 1
        check_indent(F)
    end
end

function indent(x, F)
    for (i, a) in enumerate(x)
        indent(a, F)
    end
end

function indent(x::EXPR{T}, F) where T <: Union{cst.Begin,cst.Quote,cst.While,cst.FunctionDef,cst.Macro,cst.Struct,cst.Mutable,cst.If,cst.Try,cst.Let,cst.For}
    doindent = is_multi_line(x, F) && T in F.config.IndentEXPR
    for (i, a) in enumerate(x)
        if a isa EXPR{cst.Block} && !isnestedifblock(a) && !(T == cst.For && i == 2)
            indent_block(a, F, doindent)
        else
            indent(a, F)
        end
    end
end

function indent(x::T, F::FormatState) where T <: Union{IDENTIFIER,PUNCTUATION,<:KEYWORD,LITERAL,OPERATOR}
    format(x, F)
end

end
