import Distributions.I

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

nothing
