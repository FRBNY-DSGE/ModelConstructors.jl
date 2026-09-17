using Test, ModelConstructors, Distributions, Random, LinearAlgebra, BenchmarkTools
import Distributions.I

# Set `run_benchmarks = false` before including this file (e.g. in the REPL or
# runtests.jl) to skip the benchmarks for faster testing.
if !@isdefined(run_benchmarks)
    run_benchmarks = true
end

Random.seed!(22)

i, j, k = 3, 60, 200

U = rand(InverseWishart(i + 2, Matrix(1.0I, i, i)))
V = rand(InverseWishart(j + 2, Matrix(1.0I, j, j)))
Q = rand(InverseWishart(k + 2, Matrix(1.0I, k, k)))

m = randn(i)
n = randn(j)
o = randn(k)

σ = 2.0rand()
σ² = σ ^ 2

q = MvNormal(m, σ² * U)
r = MvNormal(n, σ² * V)
s = MvNormal(o, σ² * Q)

qᴰ = DegenerateMvNormal(m, σ² * U; stdev = false)
rᴰ = DegenerateMvNormal(n, σ² * V; stdev = false)
sᴰ = DegenerateMvNormal(o, σ² * Q; stdev = false)

@testset "Test logpdf of DegenerateMvNormal at mean" begin
    @test logpdf(q, q.μ) ≈ logpdf(qᴰ, qᴰ.μ)
    @test logpdf(r, r.μ) ≈ logpdf(rᴰ, rᴰ.μ)
    @test logpdf(s, s.μ) ≈ logpdf(sᴰ, sᴰ.μ)
end

x = rand(i)
y = rand(j)
z = rand(k)

@testset "Test logpdf of DegenerateMvNormal at random location" begin
    @test logpdf(q, x) ≈ logpdf(qᴰ, x)
    @test logpdf(r, y) ≈ logpdf(rᴰ, y)
    @test logpdf(s, z) ≈ logpdf(sᴰ, z)
end

# The tests above all use `stdev = false` (which stores the covariance and sets cov=true).
# Here we exercise the default `stdev = true` constructor and the cov=false logpdf branch:
# σ is then interpreted as a standard-deviation factor L, with implied covariance Σ = L L'.
# Using a Cholesky factor of the covariance makes these match the MvNormals above.
Lq = Matrix(cholesky(Hermitian(σ² * U)).L)
Lr = Matrix(cholesky(Hermitian(σ² * V)).L)
Ls = Matrix(cholesky(Hermitian(σ² * Q)).L)

qᴸ = DegenerateMvNormal(m, Lq)  # stdev = true by default
rᴸ = DegenerateMvNormal(n, Lr)
sᴸ = DegenerateMvNormal(o, Ls)

@testset "Test logpdf of DegenerateMvNormal (stdev=true / cov=false path)" begin
    @test logpdf(q, q.μ) ≈ logpdf(qᴸ, qᴸ.μ)
    @test logpdf(r, r.μ) ≈ logpdf(rᴸ, rᴸ.μ)
    @test logpdf(s, s.μ) ≈ logpdf(sᴸ, sᴸ.μ)
    @test logpdf(q, x) ≈ logpdf(qᴸ, x)
    @test logpdf(r, y) ≈ logpdf(rᴸ, y)
    @test logpdf(s, z) ≈ logpdf(sᴸ, z)
end

if run_benchmarks
    # DegenerateMvNormal construction and logpdf across small/medium/large dimensions.
    print("DegenerateMvNormal construct (dim $i):  ")
    @btime DegenerateMvNormal($m, $σ² * $U; stdev = false)
    print("DegenerateMvNormal construct (dim $k):  ")
    @btime DegenerateMvNormal($o, $σ² * $Q; stdev = false)

    print("logpdf DegenerateMvNormal (dim $i):    ")
    @btime logpdf($qᴰ, $x)
    print("logpdf DegenerateMvNormal (dim $j):    ")
    @btime logpdf($rᴰ, $y)
    print("logpdf DegenerateMvNormal (dim $k):    ")
    @btime logpdf($sᴰ, $z)
end

nothing
