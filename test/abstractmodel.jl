using Test, ModelConstructors, BenchmarkTools

# Set `run_benchmarks = false` before including this file (e.g. in the REPL or
# runtests.jl) to skip the benchmarks for faster testing.
if !@isdefined(run_benchmarks)
    run_benchmarks = true
end

m = GenericModel()
m <= parameter(:a, 1., (0., 1.), (0., 1.), Untransformed(), fixed = false)
m <= parameter(:b, 1., (0., 1.), (0., 1.), Untransformed(), fixed = false, scaling = x -> x / 100.)

@testset "Accessing parameter values from AbstractModel" begin
    a = get_untransformed_values(m[:a])
    b = get_untransformed_values(m[:b])
    @test a == 1.
    @test b == 1e-2
end

if run_benchmarks
    print("get_untransformed_values(m[:a]): ")
    @btime get_untransformed_values($m[:a])
    print("get_untransformed_values(m[:b]): ")
    @btime get_untransformed_values($m[:b])
end
