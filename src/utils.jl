function no_trailing_ws(x, F)
    if trailing_ws_length(x) > 0
        push!(F.diagnostics, Diagnostic("Unwanted white space", [Deletion(F.offset + (length(x.span) + 1:x.fullspan))]))
    end
end

function trailing_ws(x, F)
    if trailing_ws_length(x) == 0
        push!(F.diagnostics, Diagnostic("Missing white space between operator and argument", [TextEdit(F.offset + x.fullspan + (1:0), " ")]))
    end
end

function apply(str, F::FormatState)
    edits = TextEdit[]
    for d in F.diagnostics
        append!(edits, d.edits)
    end
    sort!(edits, by = x -> -first(x.range))
    str1 = str
    for e in edits
        str1 = string(str1[1:first(e.range)-1], e.text, str1[last(e.range)+1:end])
    end
    str1
end

# returns a vector of ranges in terms of characters (not bytes)
function get_line_ranges(str::String)
    line_ranges = UnitRange{Int}[]
    i = search(str, '\n')
    n = length(str)
    if i == 0
        push!(line_ranges, 1:length(str))
    else
        push!(line_ranges, 1:ind2chr(str, i))
        while i < n
            i1 = search(str, '\n', i + 1)
            if i1 == 0
                push!(line_ranges, ind2chr(str, i + 1):length(str))
                break
            else
                push!(line_ranges, ind2chr(str, i + 1):ind2chr(str, i1 ))
                i = i1
            end
        end
    end
    return line_ranges
end

function find_line(c::Int,line_ranges::Vector{UnitRange{Int}})
    for (i, l) in enumerate(line_ranges)
        if c in l
            return i
        end
    end
    return 0
end

function find_line(r::UnitRange{Int},line_ranges::Vector{UnitRange{Int}})
    start_line = find_line(first(r), line_ranges)
    if start_line == 0
        return 0:0
    end
    end_line = find_line(last(r), line_ranges)
    if end_line == 0
        return 0:0
    end
    return start_line:end_line
end

function find_line(x::EXPR, F::FormatState)
    find_line(F.offset + (1:length(x.span)), F.line_ranges)
end

function is_multi_line(x::EXPR, F::FormatState)
    lines = find_line(x, F)
    return length(lines) > 1
end

function check_indent(F)
    target_indent = (4*F.indent)
    lr = F.line_ranges[find_line(F.offset + 1, F.line_ranges)]
    line = F.content[lr]
    if !startswith(line, " "^target_indent)
        i = target_indent - findfirst(x -> x != ' ', line) + 1
        push!(F.diagnostics, Diagnostic("Adding indent", [TextEdit(first(lr) + (0:-1), " "^i)]))
    elseif length(line) > target_indent && line[target_indent + 1] == ' '
        i = findfirst(x -> x != ' ', line) - 1
        push!(F.diagnostics, Diagnostic("Removing indent", [Deletion(first(lr) - 1 + (1:(i - target_indent)))]))
    end
end

