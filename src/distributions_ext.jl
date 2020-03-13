#=
This file defines additional functions to return objects of type Distribution. This is
necessary because we specify prior distributions wrt mean and SD
(for beta and gamma-distributed parameters) and ν and σ (for inverse gamma-distributed
parameters). Note these functions are NOT new methods for the Distributions.Beta, etc.
functions, but rather new functions with the same names.
=#
"""
```
BetaAlt(μ::T, σ::T) where {T<:Real}
```

Given μ and σ, calculate α and β and return a Distributions.Beta Distribution object.

### Arguments
`μ`: The mean of the desired distribution
`σ`: The standard deviation of the desired distribution
"""
function BetaAlt(μ::T, σ::T) where {T<:Real}
    α = (1-μ) * μ^2 / σ^2 - μ
    β = α * (1/μ - 1)
    return Distributions.Beta(α, β)
end


"""
```
GammaAlt(μ::T, σ::T) where {T<:Real}
```

Given μ and σ, calculate α and β and return a Distributions.Gamma object.

### Arguments
`μ`: The mean of the desired distribution
`σ`: The standard deviation of the desired distribution
"""
function GammaAlt(μ::T, σ::T) where {T<:Real}
    β = σ^2 / μ
    α = μ / β
    return Distributions.Gamma(α, β)
end

"""
```
mutable struct RootInverseGamma <: Distribution{Univariate, Continuous}
```

If x  ~ RootInverseGamma(ν, τ), then
   x² ~ ScaledInverseChiSquared(ν, τ²)
   x² ~ InverseGamma(ν/2, ντ²/2)

x has mode τ and ν degrees of freedom.
"""
mutable struct RootInverseGamma <: Distribution{Univariate, Continuous}
    ν::Float64
    τ::Float64
end

Distributions.params(d::RootInverseGamma) = (d.ν, d.τ)

"""
```
Distributions.pdf(d::RootInverseGamma, x::T) where {T<:Real}
```

Compute the pdf of a RootInverseGamma distribution at x.
"""
function Distributions.pdf(d::RootInverseGamma, x::T) where {T<:Real}
    (ν, τ) = params(d)
    return 2 * (ν*τ^2/2)^(ν/2) * exp((-ν*τ^2)/(2x^2)) / gamma(ν/2) / x^(ν+1)
end

"""
```
Distributions.logpdf(d::RootInverseGamma, x::T) where {T<:Real}
```

Compute the log pdf of a RootInverseGamma distribution at x.
"""
function Distributions.logpdf(d::RootInverseGamma, x::T) where {T<:Real}
    (ν, τ) = params(d)
    return log(2) - log(gamma(ν/2)) + (ν/2)*log(ν*τ^2/2) - ((ν+1)/2)*log(x^2) - ν*τ^2/(2x^2)
end

"""
```
Distributions.rand(d::RootInverseGamma; cc::T = 1.0) where T <: AbstractFloat
```

Generate a draw from the RootInverseGamma distribution `d`.
"""
function Distributions.rand(d::RootInverseGamma; cc::T = 1.0) where T<:AbstractFloat
    return sqrt(d.ν * d.τ^2 / sum(randn(round(Int,d.ν)).^2))
end

"""
```
DegenerateMvNormal <: Distribution{Multivariate, Continuous}
```

The `DegenerateMvNormal` mutable struct implements a degenerate multivariate normal
distribution. The covariance matrix may not be full rank (hence degenerate).

See [Multivariate normal distribution - Degenerate case](en.wikipedia.org/wiki/Multivariate_normal_distribution#Degenerate_case).
"""
mutable struct DegenerateMvNormal <: Distribution{Multivariate, Continuous}
    μ::Vector        # mean
    σ::Matrix        # standard deviation
    σ_inv::Matrix    # inverse of variance-covariance matrix
    λ_vals::Vector   # eigenvalues of σ
end

"""
```
DegenerateMvNormal(μ::Vector, σ::Matrix)
```
Constructor for DegenerateMvNormal type.
"""
function DegenerateMvNormal(μ::Vector, σ::Matrix)
    return DegenerateMvNormal(μ, σ, Matrix{eltype(μ)}(undef,0,0), Vector{eltype(μ)}(undef,0))
end

"""
```
function init_deg_mvnormal(μ::Vector, σ::Matrix)
```
Initializes fields for DegenerateMvNormal type.
"""
function init_deg_mvnormal(μ::Vector, σ::Matrix)
    U, λ_vals, Vt = svd(σ)
    λ_inv = [λ > 1e-6 ? 1/λ : 0.0 for λ in λ_vals]
    σ_inv = Vt' * Diagonal(λ_inv) * U'
    return DegenerateMvNormal(μ, σ, σ_inv, λ_vals)
end

"""
```
Distributions.logpdf(d::DegenerateMvNormal)
```

Method bypasses Distributions package implementation of logpdf so as to minimize numerical error.
"""
function Distributions.logpdf(d::DegenerateMvNormal, x::Vector{T}) where T<:Real
    # Occurs if one initialized a DegenerateMvNormal w/o storing inverted covariance matrix
    if isempty(d.σ_inv)
        d.σ_inv = inv(d.σ)
        λ_all, _ = eigen(d.σ)
        d.λ_vals = filter(x -> x>1e-6, λ_all)
    end
    return -(length(d.μ) * log2π + sum(log.(d.λ_vals)) + ((x .- d.μ)'*d.σ_inv*(x .- d.μ))) / 2.0
end

"""
```
rank(d::DegenerateMvNormal)
```

Returns the rank of `d.σ`.
"""
function LinearAlgebra.rank(d::DegenerateMvNormal)
    return rank(d.σ)
end

"""
```
length(d::DegenerateMvNormal)
```

Returns the dimension of `d`.
"""
Base.length(d::DegenerateMvNormal) = length(d.μ)

"""
```
Distributions.rand(d::DegenerateMvNormal; cc::T = 1.0) where T<:AbstractFloat
```

Generate a draw from `d` with variance optionally scaled by `cc^2`.
"""
function Distributions.rand(d::DegenerateMvNormal; cc::T = 1.0) where T<:AbstractFloat
    return d.μ + cc*d.σ*randn(length(d))
end

"""
```
Distributions.rand(d::DegenerateMvNormal, n::Int)
```

Generate `n` draws from `d`. This returns a matrix of size `(length(d), n)`,
where each column is a sample.
"""
function Distributions.rand(d::DegenerateMvNormal, n::Int)
    return d.μ .+ d.σ*randn(length(d), n)
end

"""
```
DegenerateDiagMvTDist <: Distribution{Multivariate, Continuous}
```

The `DegenerateDiagMvTDist` mutable struct implements a degenerate multivariate Student's t
distribution, where the covariance matrix is diagonal. The covariance matrix may
not be full rank (hence degenerate).
"""
mutable struct DegenerateDiagMvTDist <: Distribution{Multivariate, Continuous}
    μ::Vector          # mean
    σ::Matrix          # standard deviation
    ν::Int             # degrees of freedom

    function DegenerateDiagMvTDist(μ::Vector, σ::Matrix, ν::Int)
        ν > 0       ? nothing : error("Degrees of freedom (ν) must be positive")
        isdiag(σ^2) ? nothing : error("Covariance matrix (σ^2) must be diagonal")
        return new(μ, σ, ν)
    end
end

"""
```
LinearAlgebra.rank(d::DegenerateDiagMvTDist)
```

Returns the rank of `d.σ`.
"""
function rank(d::DegenerateDiagMvTDist)
    return rank(d.σ)
end

"""
```
length(d::DegenerateDiagMvTDist)
```

Returns the dimension of `d`.
"""
Base.length(d::DegenerateDiagMvTDist) = length(d.μ)

"""
```
Distributions.rand(d::DegenerateDiagMvTDist)
```

Generate a draw from `d`.
"""
function Distributions.rand(d::DegenerateDiagMvTDist)
    return d.μ + d.σ*rand(TDist(d.ν), length(d))
end

"""
```
Distributions.rand(d::DegenerateDiagMvTDist, n::Int)
```

Generate `n` draws from `d`. This returns a matrix of size `(length(d), n)`,
where each column is a sample.
"""
function Distributions.rand(d::DegenerateDiagMvTDist, n::Int)
    return d.μ .+ d.σ*rand(TDist(d.ν), length(d), n)
end

# Compute the mean μ and standard deviation σ of a RootInverseGamma object.

# A Root Inverse Gamma / Nagasaki Scaled Chi2 Distribution's mean and standard deviation
# can be computed as follows:

#     μ = √(β) * Γ(α - 0.5) / Γ(α)
#     σ = √(β / (α - 1) - μ²)

# where α = ν/2 and β = τ²*ν/2.
function mean(dist::RootInverseGamma)
    α = dist.ν/2
    β = dist.τ^2 * dist.ν/2

    μ = β^(.5) * gamma(α - 0.5) / gamma(α)
    return μ
end

function std(dist::RootInverseGamma)
    μ = mean(dist)
    α = dist.ν/2
    β = dist.τ^2 * dist.ν/2

    σ = (β / (α - 1) - μ^2)^(0.5)
    return σ
end

"""
```
MatrixNormal <: Distribution{Matrixvariate, Continuous}
```

The `MatrixNormal` mutable struct implements a matrixvariate normal
distribution. Note that the matrix must be square.

See [Matrix normal distribution - Degenerate case](en.wikipedia.org/wiki/Matrix_normal_distribution).
"""
mutable struct MatrixNormal <: Distribution{Matrixvariate, Continuous}
    μ::Matrix # mean
    U::Matrix # row variance
    V::Matrix # col variance
    U_sqrt::Matrix # cholesky of U
    V_sqrt::Matrix # choleksy of V
    U_inv::Matrix # inverse of U
    V_inv::Matrix # inverse of V

    function MatrixNormal(μ::Matrix, U::Matrix, V::Matrix)
        size(μ,1) == size(U,1) || error("μ and U must have the same number of rows")
        size(μ,2) == size(V,1) || error("μ and V must have the same number of columns")
        isposdef(U) || error("U is not a positive definite matrix")
        isposdef(V) || error("V is not a positive definite matrix")

        U_sqrt = Matrix(chol(U))
        V_sqrt = Matrix(chol(V))
        U_inv = inv(U)
        V_inv = inv(V)

        return new(μ, U, V, U_sqrt, V_sqrt, U_inv, V_inv)
    end

    function MatrixNormal(μ::Matrix, Σ::Matrix)
        size(μ) == size(Σ) || error("μ and Σ must be the same size")
        isposdef(Σ) || error("Σ is not a positive definite matrix")
        Σ_sqrt = Matrix(chol(U))
        Σ_inv = inv(U)

        return new(μ, Σ, Σ, Σ_sqrt, Σ_sqrt, Σ_inv, Σ_inv)
    end

end

"""
```
size(d::MatrixNormal)
```

Returns the dimension of `d`.
"""
Base.size(d::MatrixNormal) = size(d.μ)

"""
```
Distributions.rand(d::MatrixNormal)
```

Generate a draw from `d`.
"""
function Distributions.rand(d::MatrixNormal)
    return d.μ + d.U_sqrt*randn(size(d.μ))*d.V_sqrt'
end


function mean(d::MatrixNormal)
    return d.μ
end

Distributions.params(d::MatrixNormal) = d.μ, d.U, d.V

"""
```
Distributions.pdf(d::MatrixNormal, x::Matrix)
```

Compute the pdf of a MatrixNormal distribution at x.
"""
function Distributions.pdf(d::MatrixNormal, x::Matrix)
    μ, U, V = params(d)
    n, p = size(μ)
    U_inv = d.U_inv
    V_inv = d.V_inv
    return exp(-.5*trace(V_inv * (x-μ)' * U_inv * (x-μ))) / ((2π)^(n*p/2) * det(U)^(p/2) * det(V)^(n/2))
end

"""
```
Distributions.logpdf(d::MatrixNormal, x::Matrix)
```

Compute the logpdf of a MatrixNormal distribution at x.
"""
function Distributions.logpdf(d::MatrixNormal, x::Matrix)
    μ, U, V = params(d)
    n, p = size(μ)
    U_inv = d.U_inv
    V_inv = d.V_inv
    return -.5*trace(V_inv * (x-μ)' * U_inv * (x-μ)) - log((2π)^(n*p/2) * det(U)^(p/2) * det(V)^(n/2))
end

"""
```
truncmean(Dist::Truncated)
```
Computes the mean of truncated continuous and discrete distributions.
Written by person in this [comment](https://github.com/JuliaStats/Distributions.jl/issues/709).
Thanks, dude!
"""
function truncmean(Dist::Truncated)
    F(x) = x*pdf(Dist,x)
    y = 0.0;

    if typeof(Dist) <: ContinuousDistribution # Continuous distribution
        y = quadgk(F, Dist.lower, Dist.upper)[1]

    else # Discrete distriubtion
        x = ceil(Dist.lower)
        q_max = 1 - 1E-9;
        x_max = min(Dist.upper, quantile(Dist.untruncated, q_max))

        while x < x_max
            y += F(x)
            x += 1
        end
    end
    return y
end
