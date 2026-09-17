using Test, ModelConstructors, Dates, SparseArrays, BenchmarkTools

# Set `run_benchmarks = false` before including this file (e.g. in the REPL or
# runtests.jl) to skip the benchmarks for faster testing.
if !@isdefined(run_benchmarks)
    run_benchmarks = true
end

@testset "abbrev_symbol" begin
    @test ModelConstructors.abbrev_symbol(:abcdef) == "abcd"     # longer than default 4
    @test ModelConstructors.abbrev_symbol(:abc) == "abc"          # shorter than 4
    @test ModelConstructors.abbrev_symbol(:abcd) == "abcd"        # exactly 4
    @test ModelConstructors.abbrev_symbol(:abcdef, 6) == "abcdef" # custom n, fits
    @test ModelConstructors.abbrev_symbol(:abcdef, 3) == "abc"    # custom n, truncates
end

@testset "sorted_list_insert!" begin
    v = [1, 3, 5]
    ModelConstructors.sorted_list_insert!(v, 4)
    @test v == [1, 3, 4, 5]

    ModelConstructors.sorted_list_insert!(v, 0)
    @test v == [0, 1, 3, 4, 5]

    ModelConstructors.sorted_list_insert!(v, 10)
    @test v == [0, 1, 3, 4, 5, 10]
end

@testset "quarter_range" begin
    t0 = Date(2020, 3, 31)
    t1 = Date(2020, 12, 31)
    qr = ModelConstructors.quarter_range(t0, t1)
    @test length(qr) == 4
    @test qr[1] == Date(2020, 3, 31)
    @test qr[end] == Date(2020, 12, 31)

    # Single quarter
    qr2 = ModelConstructors.quarter_range(Date(2020, 3, 31), Date(2020, 3, 31))
    @test length(qr2) == 1
end

@testset "detexify" begin
    # Lowercase Greek
    @test ModelConstructors.detexify("α") == "alpha"
    @test ModelConstructors.detexify("β") == "beta"
    @test ModelConstructors.detexify("π") == "pi"
    @test ModelConstructors.detexify("σ") == "sigma"

    # Uppercase Greek
    @test ModelConstructors.detexify("Σ") == "Sigma"
    @test ModelConstructors.detexify("Γ") == "Gamma"

    # No unicode — unchanged
    @test ModelConstructors.detexify("hello") == "hello"

    # Symbol method
    @test ModelConstructors.detexify(:α) == :alpha
    @test ModelConstructors.detexify(:β) == :beta
end

@testset "speye" begin
    S = ModelConstructors.speye(3)
    @test S isa SparseMatrixCSC{Float64}
    @test size(S) == (3, 3)
    @test Matrix(S) == I(3)

    S2 = ModelConstructors.speye(Float32, 4)
    @test S2 isa SparseMatrixCSC{Float32}
    @test size(S2) == (4, 4)
end

@testset "n_param_regs" begin
    m = GenericModel()
    m <= parameter(:α, 1., (-1e3, 1e3), (-1e3, 1e3), Untransformed(), fixed = false)
    m <= parameter(:β, 1., (-1e3, 1e3), (-1e3, 1e3), Untransformed(), fixed = false)

    # No regimes — each returns 1
    @test ModelConstructors.n_param_regs(m.parameters) == [1, 1]

    # Set two regimes on α
    ModelConstructors.set_regime_val!(m[:α], 1, 1.)
    ModelConstructors.set_regime_val!(m[:α], 2, 2.)
    @test ModelConstructors.n_param_regs(m.parameters) == [2, 1]
end

@testset "find_param_ind and find_ind_param" begin
    m = GenericModel()
    m <= parameter(:α, 1., (-1e3, 1e3), (-1e3, 1e3), Untransformed(), fixed = false)
    m <= parameter(:β, 1., (-1e3, 1e3), (-1e3, 1e3), Untransformed(), fixed = false)

    @test ModelConstructors.find_param_ind(m.parameters, :α) == 1
    @test ModelConstructors.find_param_ind(m.parameters, :β) == 2
    @test ModelConstructors.find_param_ind(m.parameters, :missing_param) == -1

    @test ModelConstructors.find_ind_param(m.parameters, 1) == (1, 1)
    @test ModelConstructors.find_ind_param(m.parameters, 2) == (2, 1)
end

@testset "test_matrix_eq2" begin
    a = [1.0 2.0; 3.0 4.0]
    b = [1.0 2.0; 3.0 4.0]
    @test ModelConstructors.test_matrix_eq2(a, b, "a", "b")

    # Within tolerance
    c = a .+ 1e-7
    @test ModelConstructors.test_matrix_eq2(a, c, "a", "c")

    # Outside tolerance
    d = a .* 2
    @test !ModelConstructors.test_matrix_eq2(a, d, "a", "d")

    # Length mismatch throws
    @test_throws ErrorException ModelConstructors.test_matrix_eq2(a, [1.0, 2.0], "a", "short")
end

@testset "verbose print" begin
    # println/print write to stdout (not stderr), so @test_nowarn is appropriate: they
    # print when verbose >= min and are silent otherwise.
    @test_nowarn println(:high, :low, "test message")
    @test_nowarn println(:low, :high, "suppressed message")

    # info_print/warn_print emit @info/@warn (to stderr) when verbose >= min; @test_logs
    # verifies the message is emitted at the expected level.
    @test_logs (:info, "info message") ModelConstructors.info_print(:high, :low, "info message")
    @test_logs (:warn, "warn message") ModelConstructors.warn_print(:high, :low, "warn message")
end

if run_benchmarks
    print("abbrev_symbol:                        ")
    @btime ModelConstructors.abbrev_symbol(:abcdefgh)
    print("detexify:                             ")
    @btime ModelConstructors.detexify("αβγ")
end
