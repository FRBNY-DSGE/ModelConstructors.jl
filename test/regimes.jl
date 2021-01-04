using Test, ModelConstructors

u = parameter(:bloop, 2.5230, (1e-8, 5.), (1e-8, 5.), ModelConstructors.SquareRoot(); fixed = false)
ModelConstructors.set_regime_val!(u, 1, 2.5230)
# CURRENTLY ONLY TESTS VALUE, PRIOR, FIXED, AND VALUEBOUNDS SWITCHING, NO REGIME SWITCHING IN OTHER CASES
@info "The following error 'get_regime_val(), Input Error: No regime 3' is expected."
@testset "Regime switching with parameters" begin
    @test_throws ParamBoundsError ModelConstructors.set_regime_val!(u, 2, 0.)
    ModelConstructors.set_regime_val!(u, 2, 0.; override_bounds = true)
    uvec = ParameterVector{Float64}(undef, 2)
    uvec[1] = u
    uvec[2] = u

    @test !isempty(u.regimes)
    @test u.regimes[:value][1] == 2.5230
    @test u.regimes[:value][2] == 0.
    @test u.value == 2.5230
    @test ModelConstructors.regime_val(u, 1) == 2.5230
    @test ModelConstructors.regime_val(u, 2) == 0.
    @test_throws KeyError ModelConstructors.regime_val(u, 3)
    ModelConstructors.toggle_regime!(u, 2)
    @test u.value == 0.
    @test ModelConstructors.regime_val(u, 1) == 2.5230
    @test ModelConstructors.regime_val(u, 2) == 0.
    ModelConstructors.toggle_regime!(u, 1)
    @test u.value == 2.5230
    @test ModelConstructors.regime_val(u, 1) == 2.5230
    @test ModelConstructors.regime_val(u, 2) == 0.

    @test n_parameters_regime_switching(uvec) == 4
    ModelConstructors.toggle_regime!(u, 1)
    @test ModelConstructors.get_values(uvec; regime_switching = false) == [2.5230, 2.5230]
    @test ModelConstructors.get_values(uvec) == [2.5230, 2.5230, 0., 0.]

    ModelConstructors.set_regime_prior!(u, 1, Uniform(0., 5.))
    ModelConstructors.set_regime_prior!(u, 2, Normal(0., 1.))

    @test get(ModelConstructors.regime_prior(u, 1)) == Uniform(0., 5.)
    @test get(ModelConstructors.regime_prior(u, 2)) == Normal(0., 1.)

    ModelConstructors.set_regime_fixed!(u, 1, true)
    ModelConstructors.set_regime_fixed!(u, 2, false)

    @test ModelConstructors.regime_fixed(u, 1)
    @test !ModelConstructors.regime_fixed(u, 2)

    # test regime-switching value bounds
    ModelConstructors.set_regime_valuebounds!(u, 1, (1e-6, 5.))
    ModelConstructors.set_regime_valuebounds!(u, 2, (1e-7, 5.))
    @test ModelConstructors.regime_valuebounds(u, 1) == (1e-6, 5.)
    @test ModelConstructors.regime_valuebounds(u, 2) == (1e-7, 5.)

    # test set_regime_fixed w/update_valuebounds
end

@testset "Regime switching with parameters when model regimes are different" begin
    d = Dict(2 => 1, 3 => 2, 4 => 3)
    @test_throws ParamBoundsError ModelConstructors.set_regime_val!(u, 3, 0., d)
    ModelConstructors.set_regime_val!(u, 3, 0., d; override_bounds = true)
    uvec = ParameterVector{Float64}(undef, 2)
    uvec[1] = u
    uvec[2] = u

    @test !isempty(u.regimes)
    @test u.regimes[:value][1] == 2.5230
    @test u.regimes[:value][2] == 0.
    @test u.value == 2.5230
    @test ModelConstructors.regime_val(u, 2, d) == 2.5230
    @test ModelConstructors.regime_val(u, 3, d) == 0.
    @test_throws KeyError ModelConstructors.regime_val(u, 4, d)
    ModelConstructors.toggle_regime!(u, 3, d)
    @test u.value == 0.
    @test ModelConstructors.regime_val(u, 2, d) == 2.5230
    @test ModelConstructors.regime_val(u, 3, d) == 0.
    ModelConstructors.toggle_regime!(u, 2, d)
    @test u.value == 2.5230
    @test ModelConstructors.regime_val(u, 2, d) == 2.5230
    @test ModelConstructors.regime_val(u, 3, d) == 0.

    @test n_parameters_regime_switching(uvec) == 4
    ModelConstructors.toggle_regime!(u, 2, d)
    @test ModelConstructors.get_values(uvec; regime_switching = false) == [2.5230, 2.5230]
    @test ModelConstructors.get_values(uvec) == [2.5230, 2.5230, 0., 0.]

    ModelConstructors.set_regime_prior!(u, 2, Uniform(0., 5.), d)
    ModelConstructors.set_regime_prior!(u, 3, Normal(0., 1.), d)

    @test get(ModelConstructors.regime_prior(u, 1)) == Uniform(0., 5.)
    @test get(ModelConstructors.regime_prior(u, 2)) == Normal(0., 1.)
    @test get(ModelConstructors.regime_prior(u, 2, d)) == Uniform(0., 5.)
    @test get(ModelConstructors.regime_prior(u, 3, d)) == Normal(0., 1.)

    ModelConstructors.set_regime_fixed!(u, 2, true, d)
    ModelConstructors.set_regime_fixed!(u, 3, false, d)

    @test ModelConstructors.regime_fixed(u, 1)
    @test !ModelConstructors.regime_fixed(u, 2)
    @test ModelConstructors.regime_fixed(u, 2, d)
    @test !ModelConstructors.regime_fixed(u, 3, d)

    # test regime-switching value bounds
    ModelConstructors.set_regime_valuebounds!(u, 2, (1e-6, 5.), d)
    ModelConstructors.set_regime_valuebounds!(u, 3, (1e-7, 5.), d)
    ModelConstructors.set_regime_valuebounds!(u, 4, (1e-8, 5.), d)
    @test ModelConstructors.regime_valuebounds(u, 1) ==
        ModelConstructors.regime_valuebounds(u, 2, d) == (1e-6, 5.)
    @test ModelConstructors.regime_valuebounds(u, 2) ==
        ModelConstructors.regime_valuebounds(u, 3, d) == (1e-7, 5.)
    @test ModelConstructors.regime_valuebounds(u, 3) ==
        ModelConstructors.regime_valuebounds(u, 4, d) == (1e-8, 5.)

    # test set_regime_fixed w/update_valuebounds
end

nothing
