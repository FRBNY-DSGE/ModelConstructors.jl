using Test, ModelConstructors, Dates, BenchmarkTools

@testset "default_settings!" begin
    m = LinearRegression()

    # Required keys exist
    @test haskey(m.settings, :saveroot)
    @test haskey(m.settings, :dataroot)
    @test haskey(m.settings, :data_vintage)
    @test haskey(m.settings, :data_id)

    # Values
    @test m.settings[:data_id].value == 3
    @test m.settings[:data_vintage].value == Dates.format(now(), "yyyymmdd")
    @test m.settings[:saveroot].value isa String
    @test m.settings[:dataroot].value isa String
end

@testset "default_test_settings!" begin
    m = LinearRegression()

    # Required keys exist
    @test haskey(m.test_settings, :saveroot)
    @test haskey(m.test_settings, :dataroot)
    @test haskey(m.test_settings, :data_vintage)

    # Test vintage is REF
    @test m.test_settings[:data_vintage].value == "REF"

    # Saveroot is a real temp directory
    @test isdir(m.test_settings[:saveroot].value)
end

display(@benchmark LinearRegression())
