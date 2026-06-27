using ModelConstructors, Test, Random, BenchmarkTools

# Set `run_benchmarks = false` before including this file (e.g. in the REPL or
# runtests.jl) to skip the benchmarks for faster testing.
if !@isdefined(run_benchmarks)
    run_benchmarks = true
end

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

@testset "Prior computation, updating, and sampling with regime-switching" begin
    p = ParameterVector{Float64}(undef, 3)
    p[1] = ModelConstructors.parameter(:a, 0.1, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.5, 1.); fixed = false)
    p[2] = ModelConstructors.parameter(:b, 0., (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.5, 1.); fixed = true)
    p[3] = ModelConstructors.parameter(:c, 0.4, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.25, 1.); fixed = false)
    p3copy = ModelConstructors.parameter(:c, 0.5, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.25, 1.); fixed = false)

    # Set up parameter switching
    set_regime_val!(p[1], 1, 0.1) # test prior switching
    set_regime_val!(p[1], 2, 0.2)
    set_regime_val!(p[1], 3, 0.3)
    set_regime_prior!(p[1], 1, Normal(0.5, 1.))
    set_regime_prior!(p[1], 2, Normal(0.2, 1.))
    set_regime_prior!(p[1], 3, Normal(0.2, 1.))
    set_regime_val!(p[2], 1, 0.1; override_bounds = true)
    set_regime_val!(p[2], 2, 0.2; override_bounds = true)
    set_regime_fixed!(p[2], 1, true)
    set_regime_fixed!(p[2], 2, true)
    @test set_regime_val!(p[2], 2, 0.2) == 0.2 # trying to reset a fixed value won't do anything
    @test regime_val(p[2], 2) == 0.2
    set_regime_val!(p[3], 1, 0.4)
    set_regime_val!(p[3], 2, 0.5; override_bounds = true)
    set_regime_fixed!(p[3], 1, true)
    set_regime_fixed!(p[3], 2, false)

    @test !ModelConstructors._filter_all_fixed_para(p[1])
    @test ModelConstructors._filter_all_fixed_para(p[2])
    @test !ModelConstructors._filter_all_fixed_para(p[3])
    set_regime_fixed!(p[3], 2, true)
    @test ModelConstructors._filter_all_fixed_para(p[3])
    set_regime_fixed!(p[3], 2, false)
    set_regime_valuebounds!(p[3], 2, (0., 1.)) # set_regime_fixed!(p[3], 2, true) will change the valuebounds

    @test prior(p) == sum([logpdf(p[1]), logpdf(p3copy)])

    p_in = [0.2, 0.1, 0.5, 0.3, 0.9, 0.2, 0.6]
    toggle_regime!(p, 1)
    update!(p, p_in)
    p_out = [0.2, 0.1, 0.4, 0.3, 0.9, 0.2, 0.6]
    @test ModelConstructors.get_values(p) == p_out
    @test regime_val(p[1], 1) == 0.2
    @test regime_val(p[1], 2) == 0.3
    @test regime_val(p[1], 3) == 0.9

    Random.seed!(1793)
    out1 = ModelConstructors.rand_regime_switching(p; toggle = true)
    Random.seed!(1793)
    for para in p
        toggle_regime!(para, 1)
    end
    out2 = rand(p; regime_switching = true, toggle = false)
    update!(p, out1)

    @test out1 == out2
    @test regime_val(p[1], 1) == out1[1]
    @test regime_val(p[1], 2) == out1[4]
    @test regime_val(p[1], 3) == out1[5]
    @test regime_val(p[2], 1) == 0.1
    @test regime_val(p[2], 2) == 0.2
    @test regime_val(p[3], 1) == 0.4
    @test regime_val(p[3], 2) == out1[end]
end

@testset "posterior! with sampler=true (bounds handling)" begin
    p = ParameterVector{Float64}(undef, 3)
    p[1] = ModelConstructors.parameter(:a, 0.1, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.5, 1.); fixed = false)
    p[2] = ModelConstructors.parameter(:b, 0., (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.5, 1.); fixed = true)
    p[3] = ModelConstructors.parameter(:c, 0.4, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.25, 1.); fixed = false)

    loglh = (x, data) -> 2.
    data = rand(2, 2)

    # In-bounds draw: exercises the sampler try/update! path and returns a finite posterior.
    @test isfinite(posterior!(loglh, p, [0.2, 0.0, 0.5], data; sampler = true, ϕ_smc = 3.104))

    # Out-of-bounds draw (a = 2.0 ∉ (0, 1)): update! throws ParamBoundsError, which the
    # sampler path catches and converts to -Inf.
    @test posterior!(loglh, p, [2.0, 0.0, 0.5], data; sampler = true) == -Inf

    # A non-ParamBoundsError (here a length mismatch → BoundsError) is re-thrown, not
    # swallowed into -Inf.
    @test_throws BoundsError posterior!(loglh, p, [0.2, 0.0], data; sampler = true)
end

if run_benchmarks
    # Fresh (non-regime-switching) ParameterVector so the benchmarks don't depend on the
    # mutated test globals above.
    bench_p = ParameterVector{Float64}(undef, 3)
    bench_p[1] = ModelConstructors.parameter(:a, 0.1, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.5, 1.); fixed = false)
    bench_p[2] = ModelConstructors.parameter(:b, 0., (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.5, 1.); fixed = true)
    bench_p[3] = ModelConstructors.parameter(:c, 0.4, (0., 1.), (0., 1.), ModelConstructors.Untransformed(), Normal(0.25, 1.); fixed = false)
    bench_loglh = (x, data) -> 2.
    bench_data = rand(2, 2)
    bench_vals = ModelConstructors.get_values(bench_p)

    print("prior:                                ")
    @btime ModelConstructors.prior($bench_p)
    print("posterior:                            ")
    @btime posterior($bench_loglh, $bench_p, $bench_data; ϕ_smc = 3.104)
    print("posterior!:                           ")
    @btime posterior!($bench_loglh, $bench_p, $bench_vals, $bench_data; ϕ_smc = 3.104)
end

nothing
