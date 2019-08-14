using ModelConstructors

### Estimate Sharpe's single factor model
# R_{it} = α_i + β_i R_{Mt} + ϵ_{it}, i = 1,...,N; t = 1,...,T
# where R_{Mt} is the excess return on a market index in time period t,
# and ϵ_{it} is an i.i.d. normally distributed mean zero shock with variance σ_i^2

## Estimate with SMC
N = 3 # number of asset returns

# Construct data matrix and constant data

# Construct likelihood function:
# likelihood function is just R_{it} ∼ N(α_i + β_i R_{Mt}, σ_i)
# parameters to estimate are α_i, β_i, σ_i
# data is a time series of individual factor returns
# use S&P 500 data and a returns on a couple stocks. To get into returns,
# just take the log of the prices and regress those on each other.
function likelihood_fnct(p, d)
    # we assume the ordering of (α_i, β_i, σ_i)
    Σ = Matrix{Float64}(I,N,N)
    α = Vector{Float64}(N)
    β = Vector{Float64}(N)
    for i in 1:N
        α[i]   = p[i * 3 - 2]
        β[i]   = p[i * 3 - 2]
        Σ[i,i] = p[i * 3]^2
    end
    det_Σ = det(Σ)
    inv_Σ = inv(Σ)
    term1 = -N / 2 * log(2 * π) - 1 /2 * log(det_Σ)
    logprob = 0.
    errors = d .- α - β .* market_data
    for t in 1:size(d,2)
        logprob += term1 - 1/2 * dot(d[:,t], inv_Σ * d[:,t])
    end
    return exp(logprob)
end

# Construct parameter vector
