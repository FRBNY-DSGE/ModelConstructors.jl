"""
```
 set_regime_val!(p::Parameter{S},
    i::Int, v::S; override_bounds::Bool = false) where S <: Real
 set_regime_val!(p::Parameter{S},
    model_regime::Int, v::S, d::AbstractDict{Int, Int}; override_bounds::Bool = false) where S <: Real
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
                         i::Int, v::S; override_bounds::Bool = false) where S <: Real
    if !haskey(p.regimes, :value)
        p.regimes[:value] = OrderedDict{Int,S}()
    end

    # First check if fixed and enforce valuebounds so
    # `set_regime_val!` mirrors `parameter(p::Parameter, newvalue::S)` functionality
    if haskey(p.regimes, :fixed) && haskey(p.regimes[:fixed], i) ? regime_fixed(p, i) : false
        if haskey(p.regimes[:value], i)
            return p.regimes[:value][i]
        else # If it doesn't exist yet, then we want to set the value for this regime
            p.regimes[:value][i] = v
            if !haskey(p.regimes, :valuebounds) || !haskey(p.regimes[:valuebounds], i)
                p.regimes[:valuebounds][i] = (v, v)
            end
        end
    elseif (haskey(p.regimes, :valuebounds) && haskey(p.regimes[:valuebounds], i) ?
            ((regime_valuebounds(p, i)[1] <= v <= regime_valuebounds(p, i)[2]) || override_bounds) : false)
        p.regimes[:value][i] = v
    elseif (p.valuebounds[1] <= v <= p.valuebounds[2]) || override_bounds
        p.regimes[:value][i] = v
    elseif p.fixed
        # When parameters are initialized as non-regime-switching and fixed,
        # the valuebounds is automatically set to (p.value, p.value).
        # Unless valuebounds are set to regime-switching, it is not possible
        # to add extra regimes without using `override_bounds = true` (unless using the same value).
        # Note that the rest of ModelConstructors.jl assumes that if `p.fixed = true`
        # and haskey(p.regimes, :fixed) is false, then all regimes (if any) are also fixed.
        throw(ParamBoundsError("Parameter $(p.key) is fixed. Regimes cannot be added unless " *
                               "keyword `override_bounds` is set to `true`."))
    else
        throw(ParamBoundsError("New value of $(string(p.key)) ($(v)) is out of bounds ($(p.valuebounds))"))
    end
    return v
end

function set_regime_val!(p::Parameter{S}, model_regime::Int,
                         v::S, d::AbstractDict{Int, Int}; override_bounds::Bool = false) where S <: Real
    return set_regime_val!(p, d[model_regime], v; override_bounds = override_bounds)
end

"""
```
regime_val(p::Parameter{S}, i::Int) where S <: Real
regime_val(p::Parameter{S}, model_regime::Int, d::AbstractDict{Int, Int}) where S <: Real
```

returns the value of `p` in regime `i` for the first method
and the value of `p` in regime `d[model_regime` for the second.
"""
function regime_val(p::Parameter{S}, i::Int) where S <: Real
    if !haskey(p.regimes, :value) || !haskey(p.regimes[:value], i)
        @error "regime_val(), Input Error: No regime $(i)"
    end
    return p.regimes[:value][i]
end

regime_val(p::Parameter{S}, model_regime::Int, d::AbstractDict{Int, Int}) where S <: Real = regime_val(p, d[model_regime])

"""
```
set_regime_prior!(p::Parameter{S}, i::Int, v)
set_regime_prior!(p::Parameter{S}, model_regime::Int, v, d::AbstractDict{Int, Int})
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
function set_regime_prior!(p::Parameter, i::Int, v::S) where {S <: Union{NullablePriorUnivariate, NullablePriorMultivariate}}
    if !haskey(p.regimes, :prior)
        p.regimes[:prior] = OrderedDict{Int, Union{NullablePriorUnivariate, NullablePriorMultivariate}}()
    end
    p.regimes[:prior][i] = v

    return v
end

function set_regime_prior!(p::Parameter, i::Int, v::S) where S <: ContinuousUnivariateDistribution
    return set_regime_prior!(p, i, NullablePriorUnivariate(v))
end

function set_regime_prior!(p::Parameter, i::Int, v::S) where S <: ContinuousMultivariateDistribution
    return set_regime_prior!(p, i, NullablePriorMultivariate(v))
end

function set_regime_prior!(p::Parameter, model_regime::Int, v::S,
                           d::AbstractDict{Int, Int}) where {S <: Union{NullablePriorUnivariate, NullablePriorMultivariate}}
    return set_regime_prior!(p, d[model_regime], v)
end

function set_regime_prior!(p::Parameter, model_regime::Int,
                           v::S, d::AbstractDict{Int, Int}) where S <: ContinuousUnivariateDistribution
    return set_regime_prior!(p, model_regime, NullablePriorUnivariate(v), d)
end

function set_regime_prior!(p::Parameter, model_regime::Int, v::S,
                           d::AbstractDict{Int, Int}) where S <: ContinuousMultivariateDistribution
    return set_regime_prior!(p, model_regime, NullablePriorMultivariate(v), d)
end

"""
```
regime_prior(p::Parameter{S}, i::Int) where S <: Real
regime_prior(p::Parameter{S}, model_regime::Int, d::AbstractDict{Int, Int}) where S <: Real
```

returns the prior of `p` in regime `i` for the first method
and the prior of `p` in regime `d[model_regime]` for the second.
"""
function regime_prior(p::Parameter{S}, i::Int) where S <: Real
    if !haskey(p.regimes, :prior) || !haskey(p.regimes[:prior], i)
        @error "regime_prior(), Input Error: No regime $(i)"
    end
    return p.regimes[:prior][i]
end

regime_prior(p::Parameter{S}, model_regime::Int, d::AbstractDict{Int, Int}) where S <: Real = regime_prior(p, d[model_regime])

"""
```
set_regime_fixed!(p::Parameter{S}, i::Int, v::S; update_valuebounds::Interval = (NaN, NaN))
```

sets whether `p` is fixed in regime `i` of `p`. Set update_valuebounds to true to
set the valuebounds to match the fixed value.

The second method allows the user to pass a dictionary to permit the case where
there may be differences between the regimes of a regime-switching model and
the regimes for the parameters. For example, aside from regime-switching in parameters,
the model may also include other forms of regime-switching. To allow
estimation of regime-switching parameters in such a model, the dictionary `d`
maps each "model" regime to a "parameter" regime. In this way,
the second method specifies which "parameter" regime should be used at a given
"model" regime.
"""
function set_regime_fixed!(p::Parameter{S1}, i::Int, v::S; update_valuebounds::Interval{S1} = (NaN, NaN)) where {S <: Bool, S1 <: Real}
    if !haskey(p.regimes, :fixed)
        p.regimes[:fixed] = OrderedDict{Int, Bool}()
    end
    p.regimes[:fixed][i] = v

    if !haskey(p.regimes, :valuebounds)
        p.regimes[:valuebounds] = OrderedDict{Int, typeof(p.value)}()
    end

    if isnan(update_valuebounds[1]) || isnan(update_valuebounds[2])
        # Do nothing unless `v` is true (fixed in regime `i`) or
        # !haskey(p.regimes[:valuebounds], i) (no value for regime `i` for valuebounds dict)
        if v
            if (haskey(p.regimes, :value) ? haskey(p.regimes[:value], i) : false)
                # Regime is fixed, so valuebounds should be set to (value, value), if it exists
                p.regimes[:valuebounds][i] = (regime_val(p, i), regime_val(p, i))
            else
                error("Regime $(i) for parameter $(p.key) does not have a value. Set the value " *
                      "using `set_regime_val!` before making it a fixed value with `set_regime_fixed!`.")
            end
        elseif !haskey(p.regimes[:valuebounds], i)
            # If valuebounds is nonexistent in regime i, initialize to p.valuebounds
            p.regimes[:valuebounds][i] = p.valuebounds
        end
    else # Otherwise update the valuebounds. Note that there is no check that p.regimes[:value][i] lies inside the valuebounds
        p.regimes[:valuebounds][i] = update_valuebounds
    end

    return v
end

function set_regime_fixed!(p::Parameter, model_regime::Int, v::S,
                           d::AbstractDict{Int, Int}; update_valuebounds::Interval{S1} = (NaN, NaN)) where {S <: Bool, S1 <: Real}
    set_regime_fixed!(p, d[model_regime], v; update_valuebounds = update_valuebounds)
end

"""
```
regime_fixed(p::Parameter{S}, i::Int) where S <: Real
regime_fixed(p::Parameter{S}, model_regime::Int, d::AbstractDict{Int, Int}) where S <: Real
```

returns whether `p` is fixed in regime `i` for the first method
and whether true `p` is fixed in regime `d[model_regime]` for the second method.
"""
function regime_fixed(p::Parameter{S}, i::Int) where S <: Real
    if !haskey(p.regimes, :fixed) || !haskey(p.regimes[:fixed], i)
        @error "regime_fixed(), Input Error: No regime $(i)"
    end
    return p.regimes[:fixed][i]
end

regime_fixed(p::Parameter{S}, model_regime::Int, d::AbstractDict{Int, Int}) where S <: Real = regime_fixed(p, d[model_regime])

"""
```
set_regime_valuebounds!(p::Parameter{S}, i::Int, v::S)
```

sets valuebounds for `p` in regime `i` to `v`.

The second method allows the user to pass a dictionary to permit the case where
there may be differences between the regimes of a regime-switching model and
the regimes for the parameters. For example, aside from regime-switching in parameters,
the model may also include other forms of regime-switching. To allow
estimation of regime-switching parameters in such a model, the dictionary `d`
maps each "model" regime to a "parameter" regime. In this way,
the second method specifies which "parameter" regime should be used at a given
"model" regime.
"""
function set_regime_valuebounds!(p::Parameter, i::Int, v::Interval{S}) where {S <: Real}
    if !haskey(p.regimes, :valuebounds)
        p.regimes[:valuebounds] = OrderedDict{Int, typeof(p.value)}()
    end
    p.regimes[:valuebounds][i] = v

    return v
end

function set_regime_valuebounds!(p::Parameter, model_regime::Int, v::Interval{S},
                                 d::AbstractDict{Int, Int}) where {S <: Real}
    set_regime_valuebounds!(p, d[model_regime], v)
end

"""
```
regime_valuebounds(p::Parameter{S}, i::Int) where S <: Real
regime_valuebounds(p::Parameter{S}, model_regime::Int, d::AbstractDict{Int, Int}) where S <: Real
```

returns the `valuebounds` of `p`  in regime `i` for the first method
and the `valuebounds` of `p` in regime `d[model_regime]` for the second method.
"""
function regime_valuebounds(p::Parameter{S}, i::Int) where S <: Real
    if !haskey(p.regimes, :valuebounds) || !haskey(p.regimes[:valuebounds], i)
        @error "regime_valuebounds(), Input Error: No regime $(i)"
    end
    return p.regimes[:valuebounds][i]
end

regime_valuebounds(p::Parameter{S}, model_regime::Int, d::AbstractDict{Int, Int}) where S <: Real = regime_valuebounds(p, d[model_regime])

"""
```
toggle_regime!(p::Parameter{S}, i::Int) where S <: Real
toggle_regime!(pvec::ParameterVector{S}, i::Int) where S <: Real
toggle_regime!(p::Parameter{S}, model_regime::Int, d::AbstractDict{Int, Int}) where S <: Real
toggle_regime!(pvec::ParameterVector{S}, model_regime::Int, d::AbstractDict{Symbol, <: AbstractDict{Int, Int}}) where S <: Real
toggle_regime!(pvec::ParameterVector{S}, model_regime::Int, d::AbstractDict{Int, Int}) where S <: Real
```

changes the fields of `p` to regime `i`.

For example, if

```
p.regimes[:value] = OrderedDict{Int, Any}(1 => 1, 2 => 3)
```

then `toggle_regime!(p, 1)` will cause `p.value = 1` and `toggle_regime!(p, 2)`
will cause `p.value = 3`.

The third method allows the user to pass a dictionary to permit the case where
there may be differences between the regimes of a regime-switching model and
the regimes for the parameters. For example, aside from regime-switching in parameters,
the model may also include other forms of regime-switching. To allow
estimation of regime-switching parameters in such a model, the dictionary `d`
maps each "model" regime to a "parameter" regime. In this way,
the second method specifies which "parameter" regime should be used at a given
"model" regime.

The fourth method extends the third to a ParameterVector, with the possibility
that each parameter may have different mappings to the model regimes. Each key
of `d` corresponds to the key of a parameter, and each value of `d` is
the mapping for model regimes to the parameter regimes of `p.key`.
The fifth method is similar to the fourth but assumes
any regime-switching parameter has the same mapping from model regimes
to parameter regimes, hence the use of a common dictionary.
"""
function toggle_regime!(p::Parameter{S}, i::Int) where S <: Real
    if !isempty(p.regimes)
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
                    p.transform = p.regimes[field][i]
                elseif field == :prior
                    p.prior = p.regimes[field][i]
                elseif field == :fixed
                    p.fixed = p.regimes[field][i]
                end
            elseif haskey(p.regimes, field) && !haskey(p.regimes[field], i)
                error("Regime $i for field $field not found")
            end
        end
    end
end

function toggle_regime!(pvec::ParameterVector{S}, i::Int) where {S <: Real}
    for p in pvec
        toggle_regime!(p, i)
    end
end

toggle_regime!(p::Parameter{S}, model_regime::Int, d::AbstractDict{Int, Int}) where S <: Real = toggle_regime!(p, d[model_regime])

function toggle_regime!(pvec::ParameterVector{S}, model_regime::Int, d::AbstractDict{Symbol, <: AbstractDict{Int, Int}}) where S <: Real
    for p in pvec
        if haskey(d, p.key)
            toggle_regime!(p, model_regime, d[p.key])
        end
    end
end

toggle_regime!(pvec::ParameterVector{S}, model_regime::Int, d::AbstractDict{Int, Int}) where {S <: Real} = toggle_regime!(pvec, d[model_regime])

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
            vals = [x.value for x in pvec]
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
