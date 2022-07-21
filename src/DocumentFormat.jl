module DocumentFormat

import FilePathsBase
import JuliaFormatter

export format, isformatted

function default_juliaformatter_config()
    return (
        indent=4,
        annotate_untyped_fields_with_any=false,
        join_lines_based_on_source=true,
        trailing_comma=nothing,
        margin=10_000,
        always_for_in=nothing,
        whitespace_in_kwargs=false
    )
end

function format(path::FilePathsBase.AbstractPath)
    if isdir(path)
        JuliaFormatter.format(string(path), JuliaFormatter.MinimalStyle())
    else
        error("Invalid path.")
    end

    return nothing
end

function isformatted(path::FilePathsBase.AbstractPath)
    if isdir(path)
        return JuliaFormatter.format(string(path), JuliaFormatter.MinimalStyle(); overwrite=false)
    else
        error("Invalid path.")
    end
end

end
