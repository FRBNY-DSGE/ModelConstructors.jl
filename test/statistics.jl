using ModelConstructors, Test, Random
#=
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
=#

@testset "Prior computation with regime-switching" begin
end
p = ParameterVector{Float64}(undef, 3)
p[1] = ModelConstructors.parameter(:a, 0.1, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.5, 1.); fixed = false)
p[2] = ModelConstructors.parameter(:b, 0., (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.5, 1.); fixed = true)
p[3] = ModelConstructors.parameter(:c, 0.4, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.25, 1.); fixed = false)
p3cop = ModelConstructors.parameter(:c, 0.5, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.25, 1.); fixed = false)

# Set up parameter switching
set_regime_val!(p[1], 1, 0.1) # test prior switching
set_regime_val!(p[1], 2, 0.2)
set_regime_val!(p[1], 3, 0.3)
set_regime_prior!(p[1], 1, Normal(0.5, 1.))
set_regime_prior!(p[1], 2, Normal(0.2, 1.))
set_regime_prior!(p[1], 3, Normal(0.2, 1.))
set_regime_fixed!(p[2], 1, true) # test if :fixed is not in regimes, then fixed = true fixes the parameter
set_regime_fixed!(p[2], 2, true) # Need to add this info first so set_regime_val! works.
set_regime_val!(p[2], 1, 0.1)      # Alternatively, override_bounds could be used
set_regime_val!(p[2], 2, 0.2)
@test set_regime_val!(p[2], 2, 0.2) == 0.2 # trying to reset a fixed value won't do anything
@test regime_val(p[2], 2) == 0.2
set_regime_val!(p[3], 1, 0.4) # test if :fixed is in regimes and if there is no prior switching
set_regime_val!(p[3], 2, 0.5; override_bounds = true)
set_regime_fixed!(p[3], 1, true)
set_regime_fixed!(p[3], 2, false)

@test !ModelConstructors._filter_all_fixed_para(p[1])
@test ModelConstructors._filter_all_fixed_para(p[2])
@test !ModelConstructors._filter_all_fixed_para(p[3])
set_regime_fixed!(p[3], 2, true)
@test ModelConstructors._filter_all_fixed_para(p[3])
set_regime_fixed!(p[3], 2, false)

@test prior(p) == sum([logpdf(p[1]), logpdf(p3cop)])

p_in = [0.2, 0.1, 0.5, 0.3, 0.9, 0.2, 0.6]
for para in p
    toggle_regime!(para, 1)
end
update!(p, p_in)
p_out = [0.2, 0.1, 0.4, 0.3, 0.9, 0.2, 0.6]
@test ModelConstructors.get_values(p) == p_out
@test regime_val(p[1], 1) == 0.2
@test regime_val(p[1], 2) == 0.3
@test regime_val(p[1], 3) == 0.9

Random.seed!(1793)
out = rand_regime_switching(p; toggle = true)
Random.seed!(1793)
for para in p
    toggle_regime!(para, 1)
end
out = rand_regime_switching(p; toggle = false)
