using Test
using ModelConstructors
using Distributions

my_tests = [
            "parameters"
            ]

for test in my_tests
    test_file = string("$test.jl")
    @printf " * %s\n" test_file
    include(test_file)
end
