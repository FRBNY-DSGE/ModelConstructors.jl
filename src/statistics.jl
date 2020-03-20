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
`prior(parameters::ParameterVector{T}, x::AbstractVector{T}; isfree::Bool = false) where {T<:Number}`

Calculates log joint prior density of the vector `x ∈ ℝⁿ` using
the priors and transformations specified in `parameters`,
appropriately adjusting the prior using the Jacobian of the
transformations.

If the keyword `isfree` is true, then we assume that `x` only
corresponds to free parameters already, hence its length is potentially
shorter than the length of `parameters`.
"""
function prior(parameters::ParameterVector, x::AbstractVector{T};
               isfree::Bool = false) where {T <: Number}
    if isfree
        free_params = parameters
        free_x      = x
    else
        unfixed     = map(θ -> !θ.fixed, parameters)
        free_params = parameters[unfixed]
        free_x      = x[unfixed]
    end

    logpdfs = zero(T)
    @inbounds for i = 1:length(free_params)
        if hasprior(free_params[i])
            logpdfs += logpdf(get(free_params[i].prior), transform_to_model_space(free_params[i], free_x[i])) +
                log(abs(differentiate_transform_to_model_space(free_params[i], free_x[i])))
        end
    end
    logpdfs
end

"""
```
posterior(loglikelihood::Function, parameters::ParameterVector,
                   data::AbstractArray; ϕ_smc::Float64 = 1.)
```

Calculates and returns the log of the posterior distribution for `m.parameters`:

```
log posterior  = log likelihood + log prior + const
log Pr(Θ|data) = log Pr(data|Θ) + log Pr(Θ) + const
```

### Arguments

- `loglikelihood`: Function which takes vector of parameters and data, and
        outputs the log-loglikelihood of a parameters given data.
- `parameters`: Vector of parameters whose likelihood is to be evaluated
- `data`: Matrix of data for observables

### Optional Arguments

- `φ_smc`: a tempering factor to change the relative weighting of the prior and
     the likelihood when calculating the posterior. It is used primarily in SMC.
"""
function posterior(loglikelihood::Function, parameters::ParameterVector,
                   data::AbstractArray; ϕ_smc::Float64 = 1.)
    like = loglikelihood(parameters, data)
    post = ϕ_smc * like + prior(parameters)
    return post
end

"""
```
posterior!(loglikelihood::Function, parameters::ParameterVector,
           para_draw::Vector{T}, data::Matrix{T};
           sampler::Bool = false, catch_errors::Bool = false,
           φ_smc = 1) where {T<:AbstractFloat}
```

Evaluates the log posterior density at `parameters`.

### Arguments

- `loglikelihood`: a function which takes vector of parameters and data, and
                   outputs the log-loglikelihood of a parameters given data.

- `para_draw`: New values for the model parameters
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
function posterior!(loglikelihood::Function, parameters::ParameterVector,
                    para_draw::Vector{T}, data::Matrix{T};
                    sampler::Bool = false, catch_errors::Bool = false,
                    ϕ_smc::Float64 = 1.) where {T<:AbstractFloat}
    catch_errors = catch_errors | sampler
    if sampler
        try
            update!(parameters, para_draw)
        catch err
            if isa(err, ParamBoundsError)
                return -Inf
            else
                throw(err)
            end
        end
    else
        update!(parameters, para_draw)
    end
    return posterior(loglikelihood, parameters, data; ϕ_smc = ϕ_smc)
end
