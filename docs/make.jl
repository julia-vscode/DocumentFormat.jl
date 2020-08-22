using DocumentFormat
using Documenter

makedocs(;
    modules=[DocumentFormat],
    authors="Julia VSCode",
    repo="https://github.com/julia-vscode/DocumentFormat.jl/blob/{commit}{path}#L{line}",
    sitename="DocumentFormat.jl",
    format=Documenter.HTML(;
        prettyurls=prettyurls = get(ENV, "CI", nothing) == "true",
        # canonical="https://www.julia-vscode.org/DocumentFormat.jl",
        # assets=String[],
    ),
    pages=[
        "Home" => "index.md",
        "Syntax Reference" => "syntax.md",
    ],
)

deploydocs(;
    repo="github.com/julia-vscode/DocumentFormat.jl",
)
