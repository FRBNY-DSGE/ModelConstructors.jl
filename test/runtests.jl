using Test
using ModelConstructors
using Distributions, InteractiveUtils
using Nullables, Printf, Random

# Skip the (slow) per-file benchmarks during the suite; each test file still runs
# them when included standalone (it defaults run_benchmarks to true if undefined).
run_benchmarks = false

my_tests = [
            "parameters",
            "distributions_ext",
            "settings",
            "statistics",
            "paths",
            "regimes",
            "abstractmodel",
            "defaults",
            "generic_model",
            "linear_regression",
            "observables",
            "util"
            ]

@testset "ModelConstructors" begin
    for test in my_tests
        test_file = string("$test.jl")
        @printf " * %s\n" test_file
        @testset "$test_file" begin
            include(joinpath(@__DIR__, test_file))
        end
    end
end
