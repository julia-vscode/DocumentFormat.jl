module TestFormatConfig
using Test
using DocumentFormat
import DocumentFormat: parse_format_settings, Options, FormatConfigParserException

function parse_format_str(str)
    local tmp
    try
        tmp = tempname()
        f = open(tmp,  "w")
        print(f, str)
        close(f)
        parse_format_settings(tmp)
    finally
        rm(tmp; force = true)
    end
end

@testset "FormatConfig" begin
let config = parse_format_str(
    """
    # dsadas
    IndentWidth = 3


    UseTab =     true # comment
    TabWidth =4
    AlignAfterOpenBracket = Align
    """
    )

    @test config.IndentWidth == 3
    @test config.UseTab == true
    @test config.TabWidth == 4
    @test config.AlignAfterOpenBracket == Options.Align
end

@test_throws FormatConfigParserException parse_format_str(
    """
    UseTab = truee
    """
    )

@test_throws FormatConfigParserException parse_format_str(
    """
    IndentWidth = 2.0
    """
    )

@test_throws FormatConfigParserException parse_format_str(
    """
    IndentWidth : 2
    """
    )

@test_throws FormatConfigParserException parse_format_str(
    """
    AlignAfterOpenBracket = Alignn
    """
    )
end # testset

end