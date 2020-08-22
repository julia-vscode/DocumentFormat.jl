function readindent(io)
    indent = 0
    while !eof(io) && (p = Base.peek(io); p == 0x20 || p == 0x09)
        p == 0x20 && (indent += 1)
        p == 0x09 && (indent += 4)
        read(io, UInt8)
    end
    return indent
end

function get_lines(s::String)
    io = IOBuffer(s)
    lines = Tuple{Int,Int}[(0, readindent(io))] # startbyte, indent
    while !eof(io)
        c = read(io, Char)
        if c == '\r' && Base.peek(io) == 0x0a
            c = read(io, Char)
            push!(lines, (position(io), readindent(io)))
        elseif c == '\n'
            push!(lines, (position(io), readindent(io)))
        end
    end
    first(last(lines)) != lastindex(s) && push!(lines, (lastindex(s), 0))
    lines
end


function line_of(offset, lines)
    offset > first(last(lines)) && error()
    offset == 0 && return 1
    for i = 1:length(lines) - 1
        if lines[i][1] < offset <= lines[i + 1][1]
            return i
        end
    end
    return length(lines)
end

function issameline(offset1, offset2, lines)
    line_of(offset1, lines) == line_of(offset2, lines)
end

function get_expr(x, offset, pos=0)
    if pos > offset
        return nothing
    end
    if x.args isa Vector{EXPR}
        for a in x.args
            if pos < offset <= (pos + a.fullspan)
                return get_expr(a, offset, pos)
            end
            pos += a.fullspan
        end
    elseif pos == 0
        return x
    elseif (pos < offset <= (pos + x.fullspan))
        return x
    end
end