using ModelConstructors, Test

@testset "Prior computation" begin
    p = ParameterVector{Float64}(undef, 3)
    p[1] = ModelConstructors.parameter(:a, 0.1, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.5, 1.); fixed = false)
    p[2] = ModelConstructors.parameter(:b, 0., (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.5, 1.); fixed = true)
    p[3] = ModelConstructors.parameter(:c, 0.4, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.25, 1.); fixed = false)

    @test ModelConstructors.prior(p) == logpdf(p[1]) + logpdf(p[3])
end

@testset "Posterior computation" begin
    p = ParameterVector{Float64}(undef, 3)
    p[1] = ModelConstructors.parameter(:a, 0.1, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.5, 1.); fixed = false)
    p[2] = ModelConstructors.parameter(:b, 0., (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.5, 1.); fixed = true)
    p[3] = ModelConstructors.parameter(:c, 0.4, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.25, 1.); fixed = false)

    loglh = (x, data) -> 2.
    data = rand(2,2)

    @test posterior(loglh, p, data, ϕ_smc = 3.104) == 2. * 3.104 + ModelConstructors.prior(p)

    oldvals = map(x -> x.value, p)
    oldpost = 2. * 3.104 + ModelConstructors.prior(p)
    @test posterior!(loglh, p, oldvals, data, ϕ_smc = 3.104) == oldpost

    newvals = copy(oldvals)
    newvals[1] += 0.4 # moves value to the mean
    newvals[3] -= 0.15  # moves value to the mean
    @test posterior!(loglh, p, newvals, data, ϕ_smc = 3.104) > oldpost
end
