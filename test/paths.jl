using Test, ModelConstructors, BenchmarkTools

# Set `run_benchmarks = false` before including this file (e.g. in the REPL or
# runtests.jl) to skip the benchmarks for faster testing.
if !@isdefined(run_benchmarks)
    run_benchmarks = true
end

m = GenericModel()

# model paths. all this should work without errors
m.testing = true
addl_strings = ["foo=bar", "hat=head", "city=newyork"]
@testset "Check proper model paths" begin
    for fn in [:rawpath, :workpath, :tablespath, :figurespath]
        @eval $(fn)(m, "test")
        @eval $(fn)(m, "test", "temp")
        @eval $(fn)(m, "test", "temp", addl_strings)
    end
end

if run_benchmarks
    # Path construction: bare, with a filename, and with additional filestring pieces.
    print("rawpath (base):                       ")
    @btime rawpath($m, "test")
    print("rawpath (with filename):              ")
    @btime rawpath($m, "test", "temp")
    print("rawpath (with addl_strings):          ")
    @btime rawpath($m, "test", "temp", $addl_strings)
end
