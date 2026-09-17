using Test, ModelConstructors, Dates, BenchmarkTools

# Set `run_benchmarks = false` before including this file (e.g. in the REPL or
# runtests.jl) to skip the benchmarks for faster testing.
if !@isdefined(run_benchmarks)
    run_benchmarks = true
end

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

if run_benchmarks
    print("LinearRegression construction:        ")
    @btime LinearRegression()
end
