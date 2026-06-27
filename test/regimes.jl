using Test, ModelConstructors, BenchmarkTools

# Set `run_benchmarks = false` before including this file (e.g. in the REPL or
# runtests.jl) to skip the benchmarks for faster testing.
if !@isdefined(run_benchmarks)
    run_benchmarks = true
end

# CURRENTLY ONLY TESTS VALUE, PRIOR, FIXED, AND VALUEBOUNDS SWITCHING, NO REGIME SWITCHING IN OTHER CASES
# ALSO TESTS IF TRANSFORMS WORK CORRECTLY WITH REGIME-SWITCHING

@info "The following error 'get_regime_val(), Input Error: No regime 3' is expected."
@testset "Regime switching with parameters" begin
    u = parameter(:bloop, 2.5230, (1e-8, 5.), (1e-8, 5.), ModelConstructors.SquareRoot(); fixed = false)
    ModelConstructors.set_regime_val!(u, 1, 2.5230)

    @test_throws ParamBoundsError ModelConstructors.set_regime_val!(u, 2, 0.)
    ModelConstructors.set_regime_val!(u, 2, 0.; override_bounds = true)
    uvec = ParameterVector{Float64}(undef, 2)
    uvec[1] = u
    uvec[2] = u

    # Test set_regime_val! and regime_val
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

    # Test get_values and toggling regimes
    @test n_parameters_regime_switching(uvec) == 4
    ModelConstructors.toggle_regime!(u, 1)
    @test ModelConstructors.get_values(uvec; regime_switching = false) == [2.5230, 2.5230]
    @test ModelConstructors.get_values(uvec) == [2.5230, 2.5230, 0., 0.]

    # Test set_regime_prior! and regime_prior
    ModelConstructors.set_regime_prior!(u, 1, Uniform(0., 5.))
    ModelConstructors.set_regime_prior!(u, 2, Normal(0., 1.))

    @test get(ModelConstructors.regime_prior(u, 1)) == Uniform(0., 5.)
    @test get(ModelConstructors.regime_prior(u, 2)) == Normal(0., 1.)

    # Test set_regime_fixed! and regime_fixed
    @test !haskey(u.regimes, :valuebounds)
    ModelConstructors.set_regime_fixed!(u, 1, true)
    @test haskey(u.regimes, :valuebounds)
    ModelConstructors.set_regime_fixed!(u, 2, false)
    @show u.regimes[:value]
    @test_throws ErrorException ModelConstructors.set_regime_fixed!(u, 3, true)

    @test ModelConstructors.regime_fixed(u, 1)
    @test !ModelConstructors.regime_fixed(u, 2)
    @test ModelConstructors.regime_valuebounds(u, 1) == (2.5230, 2.5230)
    @test ModelConstructors.regime_valuebounds(u, 2) == (1e-8, 5.)

    # test regime-switching value bounds
    ModelConstructors.set_regime_valuebounds!(u, 1, (1e-6, 5.))
    ModelConstructors.set_regime_valuebounds!(u, 2, (1e-7, 5.))
    @test ModelConstructors.regime_valuebounds(u, 1) == (1e-6, 5.)
    @test ModelConstructors.regime_valuebounds(u, 2) == (1e-7, 5.)

    # test set_regime_fixed w/update_valuebounds
    ModelConstructors.set_regime_fixed!(u, 1, true; update_valuebounds = (10., 11.))
    ModelConstructors.set_regime_fixed!(u, 2, false; update_valuebounds = (10., 11.))
    @test u.regimes[:valuebounds][1] == (10., 11.)
    @test u.regimes[:valuebounds][2] == (10., 11.)

    # test transform_to_real_line
    ModelConstructors.toggle_regime!(uvec, 1)
    set_regime_val!(uvec[1], 2, 1.; override_bounds = true) # break the valuebounds for now
    set_regime_val!(uvec[2], 2, 1.; override_bounds = true)
    values = ModelConstructors.get_values(uvec)
    real_vals_true = similar(values)
    real_vals_true[1] = ModelConstructors.transform_to_real_line(uvec[1], uvec[1].regimes[:value][1])
    real_vals_true[2] = ModelConstructors.transform_to_real_line(uvec[2], uvec[2].regimes[:value][1])
    real_vals_true[3] = ModelConstructors.transform_to_real_line(uvec[1], uvec[1].regimes[:value][2])
    real_vals_true[4] = ModelConstructors.transform_to_real_line(uvec[2], uvec[2].regimes[:value][2])
    real_vals1 = transform_to_real_line(uvec; regime_switching = true)
    real_vals2 = transform_to_real_line(uvec, values; regime_switching = true)
    @test real_vals1 == real_vals2 == real_vals_true

    # test transform_to_model_space
    model_vals = transform_to_model_space(uvec, real_vals1; regime_switching = true)
    @test model_vals == values
end

@testset "Regime switching with parameters when model regimes are different" begin
    u = parameter(:bloop, 2.5230, (1e-8, 5.), (1e-8, 5.), ModelConstructors.SquareRoot(); fixed = false)
    ModelConstructors.set_regime_val!(u, 1, 2.5230)

    d = Dict(2 => 1, 3 => 2, 4 => 3)
    @test_throws ParamBoundsError ModelConstructors.set_regime_val!(u, 3, 0., d)
    ModelConstructors.set_regime_val!(u, 3, 0., d; override_bounds = true)
    uvec = ParameterVector{Float64}(undef, 2)
    uvec[1] = u
    uvec[2] = u

    # Test set_regime_val! and regime_val
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

    # Test get_values and toggling regimes
    @test n_parameters_regime_switching(uvec) == 4
    ModelConstructors.toggle_regime!(u, 2, d)
    @test ModelConstructors.get_values(uvec; regime_switching = false) == [2.5230, 2.5230]
    @test ModelConstructors.get_values(uvec) == [2.5230, 2.5230, 0., 0.]

    # Test set_regime_prior! and regime_prior
    ModelConstructors.set_regime_prior!(u, 2, Uniform(0., 5.), d)
    ModelConstructors.set_regime_prior!(u, 3, Normal(0., 1.), d)

    @test get(ModelConstructors.regime_prior(u, 1)) == Uniform(0., 5.)
    @test get(ModelConstructors.regime_prior(u, 2)) == Normal(0., 1.)
    @test get(ModelConstructors.regime_prior(u, 2, d)) == Uniform(0., 5.)
    @test get(ModelConstructors.regime_prior(u, 3, d)) == Normal(0., 1.)

    # Test set_regime_fixed! and regime_fixed
    ModelConstructors.set_regime_fixed!(u, 2, true, d)
    ModelConstructors.set_regime_fixed!(u, 3, false, d)
    @test_throws ErrorException ModelConstructors.set_regime_fixed!(u, 4, true, d)

    @test ModelConstructors.regime_fixed(u, 1)
    @test !ModelConstructors.regime_fixed(u, 2)
    @test ModelConstructors.regime_fixed(u, 2, d)
    @test !ModelConstructors.regime_fixed(u, 3, d)
    @test ModelConstructors.regime_valuebounds(u, 2, d) == (2.5230, 2.5230)
    @test ModelConstructors.regime_valuebounds(u, 3, d) == (1e-8, 5.)

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
    ModelConstructors.set_regime_fixed!(u, 2, true, d; update_valuebounds = (10., 11.))
    ModelConstructors.set_regime_fixed!(u, 3, false, d; update_valuebounds = (10., 11.))
    @test u.regimes[:valuebounds][1] == (10., 11.)
    @test u.regimes[:valuebounds][2] == (10., 11.)
end

if run_benchmarks
    # Fresh regime-switching parameter / vector so the benchmarks don't depend on the
    # heavily-mutated test globals above.
    bench_u = parameter(:bloop, 2.5230, (1e-8, 5.), (1e-8, 5.), ModelConstructors.SquareRoot(); fixed = false)
    ModelConstructors.set_regime_val!(bench_u, 1, 2.5230)
    ModelConstructors.set_regime_val!(bench_u, 2, 1.0; override_bounds = true)
    bench_uvec = ParameterVector{Float64}(undef, 2)
    bench_uvec[1] = bench_u
    bench_uvec[2] = bench_u

    print("set_regime_val! (existing regime):    ")
    @btime ModelConstructors.set_regime_val!($bench_u, 2, 1.0; override_bounds = true)
    print("regime_val:                           ")
    @btime ModelConstructors.regime_val($bench_u, 2)
    print("toggle_regime!:                       ")
    @btime ModelConstructors.toggle_regime!($bench_u, 1)
    print("get_values (regime_switching):        ")
    @btime ModelConstructors.get_values($bench_uvec)
    print("transform_to_real_line (reg-switch):  ")
    @btime transform_to_real_line($bench_uvec; regime_switching = true)
end

nothing
