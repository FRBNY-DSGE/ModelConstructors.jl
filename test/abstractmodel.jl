using Test, ModelConstructors, UnPack

m = GenericModel()
m <= parameter(:a, 1., (0., 1.), (0., 1.), Untransformed(), fixed = false)
m <= parameter(:b, 1., (0., 1.), (0., 1.), Untransformed(), fixed = false, scaling = x -> x / 100.)

rand_vals = rand(3)
p=SteadyStateParameterGrid(:sspg, rand(3))
p2=SteadyStateParameterGrid(:sspg, rand(3,2))
rand1 = rand()

@testset "UnPack works with AbstractModel" begin
    @unpack a, b = m
    @test a == 1.
    @test b == 1e-2
    @unpack a = m
    @unpack b = m
    @test a == 1.
    @test b == 1e-2
end

@testset "Parameter Broadcasting works" begin
    @test m[:a] .* ones(3) == m[:a].value * ones(3)
    @test m[:a] .* rand_vals == m[:a].value * rand_vals
    @test m[:b] .* ones(3) == m[:b].scaledvalue * ones(3)
    @test m[:b] .* rand_vals == m[:b].scaledvalue * rand_vals
    @test p .* 1 == p.value
    @test p .* rand1 == p.value .* rand1
    @test p .* rand_vals == p.value .* rand_vals
end
