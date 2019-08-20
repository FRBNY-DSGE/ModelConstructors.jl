using Test
using ModelConstructors
using Distributions
using Printf

my_tests = [
            "parameters",
            "settings",
            "paths",
            ]

for test in my_tests
    test_file = string("$test.jl")
    @printf " * %s\n" test_file
    include(test_file)
end
