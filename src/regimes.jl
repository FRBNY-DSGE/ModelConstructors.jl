"""
```
 set_regime_val!(p::Parameter{S},
    i::Int64, v::S; override_bounds::Bool = false) where S <: Real
 set_regime_val!(p::Parameter{S},
    model_regime::Int64, v::S, d::AbstractDict; override_bounds::Bool = false) where S <: Real
```

sets the value in regime `i` of `p` to be `v`. By default, we enforce
the bounds that are currently in `p`, but the bounds can be ignoerd by
setting `override_bounds = true`.

The second method allows the user to pass a dictionary to permit the case where
there may be differences between the regimes of a regime-switching model and
the regimes for the parameters. For example, aside from regime-switching in parameters,
the model may also include other forms of regime-switching. To allow
estimation of regime-switching parameters in such a model, the dictionary `d`
maps each "model" regime to a "parameter" regime. In this way,
the second method specifies which "parameter" regime should be used at a given
"model" regime.
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

function set_regime_val!(p::Parameter{S}, model_regime::Int64,
                         v::S, d::AbstractDict; override_bounds::Bool = false) where S <: Real
    return set_regime_val!(p, d[model_regime], v; override_bounds = override_bounds)
end

"""
```
regime_val(p::Parameter{S}, i::Int64) where S <: Real
regime_val(p::Parameter{S}, model_regime::Int64, d::AbstractDict) where S <: Real
```

returns the value of `p` in regime `i` for the first method
and the value of `p` in regime `d[model_regime` for the second.
"""
function regime_val(p::Parameter{S}, i::Int64) where S <: Real
    if !haskey(p.regimes, :value) || !haskey(p.regimes[:value], i)
        @error "regime_val(), Input Error: No regime $(i)"
    end
    return p.regimes[:value][i]
end

function regime_val(p::Parameter{S}, model_regime::Int64, d::AbstractDict) where S <: Real
    return regime_val(p, d[model_regime])
end

"""
```
set_regime_prior!(p::Parameter{S}, i::Int64, v)
set_regime_prior!(p::Parameter{S}, model_regime::Int64, v, d::AbstractDict)
```

sets the prior in regime `i` of `p` to be `v`. The type of `v`
can be a `NullablePriorUnivariate`, `NullablePriorMultivariate`,
`ContinuousUnivariateDistribution`, or `ContinuousMultivariateDistribution'.

The second method allows the user to pass a dictionary to permit the case where
there may be differences between the regimes of a regime-switching model and
the regimes for the parameters. For example, aside from regime-switching in parameters,
the model may also include other forms of regime-switching. To allow
estimation of regime-switching parameters in such a model, the dictionary `d`
maps each "model" regime to a "parameter" regime. In this way,
the second method specifies which "parameter" regime should be used at a given
"model" regime.
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

function set_regime_prior!(p::Parameter, model_regime::Int64, v::S,
                           d::AbstractDict) where {S <: Union{NullablePriorUnivariate, NullablePriorMultivariate}}
    return set_regime_prior!(p, d[model_regime], v)
end

function set_regime_prior!(p::Parameter, model_regime::Int64,
                           v::S, d::AbstractDict) where S <: ContinuousUnivariateDistribution
    return set_regime_prior!(p, model_regime, NullablePriorUnivariate(v), d)
end

function set_regime_prior!(p::Parameter, model_regime::Int64, v::S,
                           d::AbstractDict) where S <: ContinuousMultivariateDistribution
    return set_regime_prior!(p, model_regime, NullablePriorMultivariate(v), d)
end

"""
```
regime_prior(p::Parameter{S}, i::Int64) where S <: Real
regime_prior(p::Parameter{S}, model_regime::Int64, d::AbstractDict) where S <: Real
```

returns the prior of `p` in regime `i` for the first method
and the prior of `p` in regime `d[model_regime` for the second.
"""
function regime_prior(p::Parameter{S}, i::Int64) where S <: Real
    if !haskey(p.regimes, :prior) || !haskey(p.regimes[:prior], i)
        @error "regime_prior(), Input Error: No regime $(i)"
    end
    return p.regimes[:prior][i]
end

function regime_prior(p::Parameter{S}, model_regime::Int64, d::AbstractDict) where S <: Real
    return regime_prior(p, d[model_regime])
end

"""
```
set_regime_fixed!(p::Parameter{S}, i::Int64, v::S)
```

sets whether `p` is fixed in regime `i` of `p`.

The second method allows the user to pass a dictionary to permit the case where
there may be differences between the regimes of a regime-switching model and
the regimes for the parameters. For example, aside from regime-switching in parameters,
the model may also include other forms of regime-switching. To allow
estimation of regime-switching parameters in such a model, the dictionary `d`
maps each "model" regime to a "parameter" regime. In this way,
the second method specifies which "parameter" regime should be used at a given
"model" regime.
"""
function set_regime_fixed!(p::Parameter, i::Int64, v::S) where {S <: Bool}
    if !haskey(p.regimes, :fixed)
        p.regimes[:fixed] = OrderedDict{Int64, Bool}()
    end
    p.regimes[:fixed][i] = v

    return v
end

function set_regime_fixed!(p::Parameter, model_regime::Int64, v::S,
                           d::AbstractDict) where {S <: Bool}
    set_regime_fixed!(p, d[model_regime], v)
end

"""
```
regime_fixed(p::Parameter{S}, i::Int64) where S <: Real
regime_fixed(p::Parameter{S}, model_regime::Int64, d::AbstractDict) where S <: Real
```

returns whether `p` is fixed in regime `i` for the first method
and whether true `p` is fixed in regime `d[model_regime]` for the second method.
"""
function regime_fixed(p::Parameter{S}, i::Int64) where S <: Real
    if !haskey(p.regimes, :fixed) || !haskey(p.regimes[:fixed], i)
        @error "regime_fixed(), Input Error: No regime $(i)"
    end
    return p.regimes[:fixed][i]
end

function regime_fixed(p::Parameter{S}, model_regime::Int64, d::AbstractDict) where S <: Real
    regime_fixed(p, d[model_regime])
end

"""
```
toggle_regime!(p::Parameter{S}, i::Int64) where S <: Real
toggle_regime!(p::Parameter{S}, model_regime::Int64, d::AbstractDict) where S <: Real
```

changes the fields of `p` to regime `i`.

For example, if

```
p.regimes[:value] = OrderedDict{Int, Any}(1 => 1, 2 => 3)
```

then `toggle_regime!(p, 1)` will cause `p.value = 1` and `toggle_regime!(p, 2)`
will cause `p.value = 3`.

The second method allows the user to pass a dictionary to permit the case where
there may be differences between the regimes of a regime-switching model and
the regimes for the parameters. For example, aside from regime-switching in parameters,
the model may also include other forms of regime-switching. To allow
estimation of regime-switching parameters in such a model, the dictionary `d`
maps each "model" regime to a "parameter" regime. In this way,
the second method specifies which "parameter" regime should be used at a given
"model" regime.
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
                p.fixed = p.regimes[:fixed][i]
            end
        elseif haskey(p.regimes, field) && !haskey(p.regimes[field], i)
            error("Regime $i for field $field not found")
        end
    end
end

function toggle_regime!(p::Parameter{S}, model_regime::Int64, d::AbstractDict) where S <: Real
    return toggle_regime!(p, d[model_regime])
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
