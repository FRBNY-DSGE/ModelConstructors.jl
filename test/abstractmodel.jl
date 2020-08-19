using Test, ModelConstructors, UnPack

m = GenericModel()
m <= parameter(:a, 1., (0., 1.), (0., 1.), Untransformed(), fixed = false)
m <= parameter(:b, 1., (0., 1.), (0., 1.), Untransformed(), fixed = false, scaling = x -> x / 100.)
@testset "UnPack works with AbstractModel" begin
    @unpack a, b = m
    @test a == 1.
    @test b == 1e-2
    @unpack a = m
    @unpack b = m
    @test a == 1.
    @test b == 1e-2
end
