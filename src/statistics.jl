"""
`prior(parameters::ParameterVector{T}) where {T<:Number}`

Calculates log joint prior density of m.parameters.
"""
function prior(parameters::ParameterVector{T}) where {T<:Number}
    free_params = Base.filter(θ -> !θ.fixed, parameters)
    logpdfs = map(logpdf, free_params)
    return sum(logpdfs)
end

"""
```
posterior(m::AbstractDSGEModel{T}, data::Matrix{T};
          sampler::Bool = false, catch_errors::Bool = false,
          φ_smc = 1) where {T<:AbstractFloat}
```

Calculates and returns the log of the posterior distribution for `m.parameters`:

```
log posterior  = log likelihood + log prior + const
log Pr(Θ|data) = log Pr(data|Θ) + log Pr(Θ) + const
```

### Arguments

- `m`: the model object
- `data`: matrix of data for observables

### Optional Arguments
-`sampler`: Whether metropolis_hastings or smc is the caller. If `sampler=true`,
    the log likelihood and the transition matrices for the zero-lower-bound
    period are also returned.
-`catch_errors`: Whether to catch errors of type `GensysError` or `ParamBoundsError`
- `φ_smc`: a tempering factor to change the relative weighting of the prior and
     the likelihood when calculating the posterior. It is used primarily in SMC.
"""
function posterior(m::AbstractDSGEModel{T}, data::AbstractArray;
                   sampler::Bool = false, ϕ_smc::Float64 = 1.,
                   catch_errors::Bool = false) where {T<:AbstractFloat}
    catch_errors = catch_errors | sampler
    like = likelihood(m, data; sampler=sampler, catch_errors=catch_errors)
    post = ϕ_smc*like + prior(m)
    return post
end

"""
```
posterior!(m::AbstractDSGEModel{T}, parameters::Vector{T}, data::Matrix{T};
           sampler::Bool = false, catch_errors::Bool = false,
           φ_smc = 1) where {T<:AbstractFloat}
```

Evaluates the log posterior density at `parameters`.

### Arguments

- `m`: The model object
- `parameters`: New values for the model parameters
- `data`: Matrix of input data for observables

### Optional Arguments
- `sampler`: Whether metropolis_hastings or smc is the caller. If `sampler=true`,
     the log likelihood and the transition matrices for the zero-lower-bound
     period are also returned.
- `catch_errors`: Whether to catch errors of type `GensysError` or `ParamBoundsError`
     If `sampler = true`, both should always be caught.
- `φ_smc`: a tempering factor to change the relative weighting of the prior and
     the likelihood when calculating the posterior. It is used primarily in SMC.
"""
function posterior!(m::AbstractDSGEModel{T}, parameters::Vector{T}, data::AbstractArray;
                    sampler::Bool = false, ϕ_smc::Float64 = 1.,
                    catch_errors::Bool = false) where {T<:AbstractFloat}
    catch_errors = catch_errors | sampler
    if sampler
        try
            DSGE.update!(m, parameters)
        catch err
            if isa(err, ParamBoundsError)
                return -Inf
            else
                throw(err)
            end
        end
    else
        DSGE.update!(m, parameters)
    end
    return posterior(m, data; sampler=sampler, ϕ_smc=ϕ_smc, catch_errors=catch_errors)
end
