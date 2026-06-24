using Test, ModelConstructors, BenchmarkTools

@testset "LinearRegression construction" begin
    m = LinearRegression()
    @test m isa LinearRegression{Float64}
    @test m.spec == ""
    @test m.subspec == "ss0"
    @test !m.testing

    # Custom spec and subspec
    m2 = LinearRegression("myspec", "ss1")
    @test m2.spec == "myspec"
    @test m2.subspec == "ss1"

    # Testing flag
    m3 = LinearRegression(testing = true)
    @test m3.testing
end

@testset "LinearRegression description" begin
    m = LinearRegression()
    @test ModelConstructors.description(m) == "LinearRegression model."
end

@testset "LinearRegression init_parameters!" begin
    m = LinearRegression()

    # Three parameters: α, β, σ
    @test length(m.parameters) == 3
    @test m[:α].key == :α
    @test m[:β].key == :β
    @test m[:σ].key == :σ

    # Initial values
    @test m[:α].value == 1.
    @test m[:β].value == 1.
    @test m[:σ].value == 1.

    # Bounds
    @test m[:α].valuebounds == (-1e3, 1e3)
    @test m[:β].valuebounds == (-1e3, 1e3)
    @test m[:σ].valuebounds == (0., 1e3)

    # None are fixed
    @test !m[:α].fixed
    @test !m[:β].fixed
    @test !m[:σ].fixed
end

display(@benchmark LinearRegression())
