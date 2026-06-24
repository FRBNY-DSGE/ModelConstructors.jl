using Test, ModelConstructors, BenchmarkTools

@testset "Observable construction" begin
    fwd = df -> df[:, :GDP]
    rev = x -> x
    obs = Observable(:obs_gdp, [:GDP__FRED], fwd, rev, "GDP Growth", "Real GDP Growth per capita")

    @test obs.key == :obs_gdp
    @test obs.input_series == [:GDP__FRED]
    @test obs.name == "GDP Growth"
    @test obs.longname == "Real GDP Growth per capita"
    @test obs.fwd_transform === fwd
    @test obs.rev_transform === rev
end

@testset "PseudoObservable construction" begin
    # Key-only constructor uses defaults
    po = PseudoObservable(:flex_output)
    @test po.key == :flex_output
    @test po.name == ""
    @test po.longname == ""
    @test po.rev_transform === identity

    # Full constructor
    po2 = PseudoObservable(:flex_output, "Flexible Output", "Flexible Price Output", log)
    @test po2.key == :flex_output
    @test po2.name == "Flexible Output"
    @test po2.longname == "Flexible Price Output"
    @test po2.rev_transform === log
end

display(@benchmark PseudoObservable(:flex_output))
