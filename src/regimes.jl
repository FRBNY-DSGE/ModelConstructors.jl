"""
```
 set_regime_val!(p::Parameter{S},
    i::Int64, v::S; override_bounds::Bool = false) where S <: Real
```

sets the value in regime `i` of `p` to be `v`. By default, we enforce
the bounds that are currently in `p`, but the bounds can be ignoerd by
setting `override_bounds = true`.
"""
function set_regime_val!(p::Parameter{S},
                         i::Int64, v::S; override_bounds::Bool = false) where S <: Real
    if !haskey(p.regimes, :value)
        p.regimes[:value] = OrderedDict{Int64,S}()
    end
    if p.valuebounds[1] <= v <= p.valuebounds[2] || override_bounds
        p.regimes[:value][i] = v
    else
        throw(ParamBoundsError("New value of $(string(p.key)) ($(v)) is out of bounds ($(p.valuebounds))"))
    end
    return v
end

"""
```
function regime_val(p::Parameter{S}, i::Int64) where S <: Real
```

returns the value of `p` in regime `i`.
"""
function regime_val(p::Parameter{S}, i::Int64) where S <: Real
    if !haskey(p.regimes, :value) || !haskey(p.regimes[:value], i)
        @error "get_regime_val(), Input Error: No regime $(i)"
    end
    return p.regimes[:value][i]
end

"""
```
 set_regime_prior!(p::Parameter{S}, i::Int64, v::S)
```

sets the prior in regime `i` of `p` to be `v`. The type of `v`
can be a `NullablePriorUnivariate`, `NullablePriorMultivariate`,
`ContinuousUnivariateDistribution`, or `ContinuousMultivariateDistribution'.
"""
function set_regime_prior!(p::Parameter, i::Int64, v::S) where {S <: Union{NullablePriorUnivariate, NullablePriorMultivariate}}
    if !haskey(p.regimes, :prior)
        p.regimes[:prior] = OrderedDict{Int64, Union{NullablePriorUnivariate, NullablePriorMultivariate}}()
    end
    p.regimes[:prior][i] = v

    return v
end

function set_regime_prior!(p::Parameter, i::Int64, v::S) where S <: ContinuousUnivariateDistribution
    return set_regime_prior!(p, i, NullablePriorUnivariate(v))
end

function set_regime_prior!(p::Parameter, i::Int64, v::S) where S <: ContinuousMultivariateDistribution
    return set_regime_prior!(p, i, NullablePriorMultivariate(v))
end

"""
```
function regime_prior(p::Parameter{S}, i::Int64) where S <: Real
```

returns the prior of `p` in regime `i`.
"""
function regime_prior(p::Parameter{S}, i::Int64) where S <: Real
    if !haskey(p.regimes, :prior) || !haskey(p.regimes[:prior], i)
        @error "get_regime_prior(), Input Error: No regime $(i)"
    end
    return p.regimes[:prior][i]
end

"""
```
function toggle_regime!(p::Parameter{S}, i::Int64) where S <: Real
```

changes the fields of `p` to regime `i`.

For example, if

```
p.regimes[:value] = OrderedDict{Int, Any}(1 => 1, 2 => 3)
```

then `toggle_regime!(p, 1)` will cause `p.value = 1` and `toggle_regime!(p, 2)`
will cause `p.value = 3`.
"""
function toggle_regime!(p::Parameter{S}, i::Int64) where S <: Real
    for field in [:value, :valuebounds, :transform_parameterization,
                  :transform, :prior, :fixed]
        if haskey(p.regimes, field) && haskey(p.regimes[field], i)
            if field == :value
                p.value = p.regimes[field][i]
            elseif field == :valuebounds
                p.valuebounds = p.regimes[field][i]
            elseif field == :transform_parameterization
                p.transform_parameterization = p.regimes[field][i]
            elseif field == :transform
                p.transform = p.regimes[:transform][i]
            elseif field == :prior
                p.prior = p.regimes[:prior][i]
            elseif field == :fixed
                p.transform = p.regimes[:fixed][i]
            end
        elseif haskey(p.regimes, field) && !haskey(p.regimes[field], i)
            error("Regime $i for field $field not found")
        end
    end
end

"""
```
get_values(pvec::ParameterVector{S}; regime_switching::Bool = true) where {S <: Real}
```

constructs a vector of the underlying values in a `ParameterVector`, including
if there are regime-switching values.
"""
function get_values(pvec::ParameterVector{S}; regime_switching::Bool = true) where {S <: Real}

    if regime_switching  # Check if regime switching occurs
        np_reg = n_parameters_regime_switching(pvec)
        np     = length(pvec)
        if np == np_reg # No regime-switching
            vals = [x.value for x in pvec.parameters]
        else
            vals = Vector{S}(undef, np_reg)

            # An initial pass to find regime 1 values
            for i in 1:np
                if isempty(pvec[i].regimes)
                    vals[i] = pvec[i].value
                elseif haskey(pvec[i].regimes, :value)
                    vals[i] = pvec[i].regimes[:value][1]
                end
            end

            # A second loop to add in the extra regimes
            ct = np # a counter to assist us
            for i in 1:np
                if !isempty(pvec[i].regimes)
                    if haskey(pvec[i].regimes, :value)
                        for j in 1:length(pvec[i].regimes[:value])
                            if j > 1
                                ct += 1
                                vals[ct] = pvec[i].regimes[:value][j]
                            end
                        end
                    end
                end
            end
        end
    else # Regime switching doesn't occur, so just directly map
        vals = [x.value for x in pvec]
    end

    return vals
end
