using Test, ModelConstructors, BenchmarkTools

@testset "GenericModel construction" begin
    m = GenericModel()
    @test m isa GenericModel{Float64}
    @test isempty(m.parameters)
    @test m.spec == ""
    @test m.subspec == ""
    @test !m.testing

    # Custom spec and subspec
    m2 = GenericModel("myspec", "mysubspec")
    @test m2.spec == "myspec"
    @test m2.subspec == "mysubspec"

    # Custom settings
    cs = Dict{Symbol, Setting}(:mysetting => Setting(:mysetting, 42))
    m3 = GenericModel(custom_settings = cs)
    @test m3.settings[:mysetting].value == 42

    # Testing flag
    m4 = GenericModel(testing = true)
    @test m4.testing
end

@testset "GenericModel description" begin
    m = GenericModel()
    @test ModelConstructors.description(m) isa String
    @test !isempty(ModelConstructors.description(m))
end

@testset "GenericModel parameter addition" begin
    m = GenericModel()
    m <= parameter(:α, 0.5, (0., 1.), (0., 1.), Untransformed(), fixed = false)
    @test length(m.parameters) == 1
    @test m.parameters[1].key == :α
    @test m[:α].value == 0.5

    # Add a second parameter
    m <= parameter(:β, 0.3, (0., 1.), (0., 1.), Untransformed(), fixed = false)
    @test length(m.parameters) == 2
    @test m[:β].value == 0.3
end

display(@benchmark GenericModel())
