import Base: <=

"""
```
AbstractVectorParameter{T<:Number}
```

The AbstractVectorParameter type is the common supertype of all model
parameters, including steady-state values.  Its subtype structure is
as follows:

-`AbstractParameter{T<:Number}`: The common abstract supertype for all parameters.
    -`Parameter{S<:Real,T<:Number, U<:Transform}`: The abstract supertype for parameters that are directly estimated.
        -`UnscaledParameter{S<:Real, T<:Number, U:<Transform}`: Concrete type for parameters that do not need to be scaled for equilibrium conditions.
        -`ScaledParameter{S<:Real, T<:Number, U:<Transform}`: Concrete type for parameters that are scaled for equilibrium conditions.
    -`SteadyStateParameter{T<:Number}`: Concrete type for steady-state parameters.
"""
abstract type AbstractVectorParameter{V<:Vector, T<:Number} end
abstract type VectorParameter{V,T,U<:Transform} <: AbstractVectorParameter{V,T} end
VectorParameterVector{V,T} =  Vector{AbstractVectorParameter{V,T}}

mutable struct UnscaledVectorParameter{V<:Vector,T<:Number,U<:Transform} <: VectorParameter{V,T,U}
    key::Symbol
    value::V                            # parameter value in model space
    valuebounds::Interval{T}            # bounds of parameter value
    transform_parameterization::Interval{T} # parameters for transformation
    transform::U                        # transform btw. model space & real line for optimization
    prior::NullablePriorMultivariate    # prior distribution
    fixed::Bool                         # is this parameter fixed at some value?
    description::String
    tex_label::String                   # LaTeX label for printing=#
end


"""
```
ScaledParameter{S,T,U} <: Parameter{S,T,U}
```

Time-invariant model parameter whose value is scaled for use in the model's equilibrium
conditions.

#### Fields

- `key::Symbol`: Parameter name. For maximum clarity, `key`
  should conform to the guidelines established in the DSGE Style Guide.
- `value::T`: The parameter's unscaled value. Initialized in model
  space (guaranteed to be between `valuebounds`), but can be
  transformed between model space and the real line via calls to
  `transform_to_real_line` and `transform_to_model_space`.
- `scaledvalue::T`: Parameter value scaled for use in `eqcond.jl`
- `valuebounds::Interval{T}`: Bounds for the parameter's value in model space.
- `transform_parameterization::Interval{T}`: Parameters used to
  transform `value` between model space and the real line.
- `transform::U`: The transformation used to convert `value` between model space and the
  real line, for use in optimization.
- `prior::NullablePrior`: Prior distribution for parameter value.
- `fixed::Bool`: Indicates whether the parameter's value is fixed rather than estimated.
- `scaling::Function`: Function used to scale parameter value for use in equilibrium
  conditions.
- `description::String`: A short description of the parameter's economic
  significance.
- `tex_label::String`: String for printing parameter name to LaTeX.
"""
mutable struct ScaledVectorParameter{V,T,U} <: VectorParameter{V,T,U}
    key::Symbol
    value::V
    scaledvalue::V
    valuebounds::Interval{T}
    transform_parameterization::Interval{T}
    transform::U
    prior::NullablePriorMultivariate
    fixed::Bool
    scaling::Function
    description::String
    tex_label::String
end

hasprior(p::VectorParameter) = !isnull(p.prior)

NullableOrPriorMultivariate = Union{NullablePriorMultivariate, ContinuousMultivariateDistribution}

#="""
```
parameter{S,T,U<:Transform}(key::Symbol, value::S, valuebounds = (value,value),
                          transform_parameterization = (value,value),
                          transform = Untransformed(), prior = NullablePrior();
                          fixed = true, scaling::Function = identity, description = "",
                          tex_label::String = "")
```

By default, returns a fixed `UnscaledParameter` object with key `key`
and value `value`. If `scaling` is given, a `ScaledParameter` object
is returned.
"""
function parameter(key::Symbol,
                   value::V,
                   valuebounds::Interval{T} = (value, value),
                   transform_parameterization::Interval{T} = (value, value),
                   transform::U        = Untransformed(),
                   prior::Union{NullableOrPriorUnivariate,
                                NullableOrPriorMultivariate} = NullablePriorUnivariate();
                   fixed::Bool         = true,
                   scaling::Function   = identity,
                   description::String = "No description available.",
                   tex_label::String   = "") where {V<:Vector, T<:Float64, U<:Transform}

    # If fixed=true, force bounds to match and leave prior as null.  We need to define new
    # variable names here because of lexical scoping.

    valuebounds_new = valuebounds
    transform_parameterization_new = transform_parameterization
    transform_new = transform
    U_new = U
    prior_new = prior

    if fixed
        # value[1] because need to deal with case in which value is a vector (but if only Float, [1] just takes the Float
        transform_parameterization_new = (value[1], value[1])  # value is transformed already
        transform_new = Untransformed()                 # fixed priors should stay untransformed
        U_new = Untransformed

        if isa(transform, Untransformed)
            valuebounds_new = (value[1], value[1])
        end
    else
        transform_parameterization_new = transform_parameterization
    end

    # ensure that we have a Nullable{Distribution}, if not construct one
    prior_new = if isa(prior_new, NullableOrPriorUnivariate)
        !isa(prior_new,NullablePriorUnivariate) ? NullablePriorUnivariate(prior_new) : prior_new
    elseif isa(prior_new, NullableOrPriorMultivariate)
        !isa(prior_new,NullablePriorMultivariate) ? NullablePriorMultivariate(prior_new) : prior_new
    else
        @error "Must be a PriorOrNullable or PriorOrNullableMultivariate"
    end

    if scaling == identity
        if typeof(value) <: Number #Real
            return UnscaledParameter{T,U_new}(key, value, valuebounds_new,
                                              transform_parameterization_new, transform_new,
                                              prior_new, fixed, description, tex_label) #S
        elseif typeof(value) <: Vector
            return UnscaledVectorParameter{V,T,U_new}(key, value, valuebounds_new,
                                              transform_parameterization_new, transform_new,
                                              prior_new, fixed, description, tex_label)
        else
            @error "Type of value not yet supported"
        end
    else
        if typeof(value) <: Number #Real
            return ScaledParameter{T,U_new}(key, value, scaling(value), valuebounds_new,
                                            transform_parameterization_new, transform_new,
                                            prior_new, fixed, scaling, description, tex_label)
        elseif typeof(value) <: Vector
            return ScaledVectorParameter{V,T,U_new}(key, value, scaling(value), valuebounds_new,
                                            transform_parameterization_new, transform_new,
                                            prior_new, fixed, scaling, description, tex_label)
        end
    end
end

function parameter_ad(key::Symbol,
                      value::Union{S,V},
                      valuebounds::Interval{T} = (value,value),
                      transform_parameterization::Interval{T} = (value,value),
                      transform::U             = Untransformed(),
                      prior::Union{NullableOrPriorUnivariate, NullableOrPriorMultivariate} = NullablePriorUnivariate();
                      fixed::Bool              = true,
                      scaling::Function        = identity,
                      description::String = "No description available.",
                      tex_label::String = "") where {V<:Vector, S<:Real, T <: Float64, U <:Transform}

    # If fixed=true, force bounds to match and leave prior as null.  We need to define new
    # variable names here because of lexical scoping.

    valuebounds_new = valuebounds
    transform_parameterization_new = transform_parameterization
    transform_new = transform
    U_new = U
    prior_new = prior

    if fixed
        # value[1] because need to deal with case in which value is a vector (but if only Float, [1] just takes the Float
        transform_parameterization_new = (value[1],value[1])  # value is transformed already
        transform_new = Untransformed()                 # fixed priors should stay untransformed
        U_new = Untransformed

        if isa(transform, Untransformed)
            valuebounds_new = (value[1],value[1])
        end
    else
        transform_parameterization_new = transform_parameterization
    end

    # ensure that we have a Nullable{Distribution}, if not construct one
    prior_new = if isa(prior_new, NullableOrPriorUnivariate)
        !isa(prior_new,NullablePriorUnivariate) ? NullablePriorUnivariate(prior_new) : prior_new
    elseif isa(prior_new, NullableOrPriorMultivariate)
        !isa(prior_new,NullablePriorMultivariate) ? NullablePriorMultivariate(prior_new) : prior_new
    else
        @error "Must be a PriorOrNullable or PriorOrNullableMultivariate"
    end

    if scaling == identity
        if typeof(value) <: Real
            return UnscaledParameterAD{S,T,U_new}(key, value, valuebounds_new,
                                              transform_parameterization_new, transform_new,
                                              prior_new, fixed, description, tex_label) #S
        elseif typeof(value) <: Vector
            return UnscaledVectorParameter{V,T,U_new}(key, value, valuebounds_new,
                                              transform_parameterization_new, transform_new,
                                              prior_new, fixed, description, tex_label)
        else
            @error "Type of value not yet supported"
        end
    else
        if typeof(value) <: Real
            return ScaledParameterAD{S,T,U_new}(key, value, scaling(value), valuebounds_new,
                                            transform_parameterization_new, transform_new,
                                            prior_new, fixed, scaling, description, tex_label)
        elseif typeof(value) <: Vector
            return ScaledVectorParameter{V,T,U_new}(key, value, scaling(value), valuebounds_new,
                                            transform_parameterization_new, transform_new,
                                            prior_new, fixed, scaling, description, tex_label)
        end
    end
end

function parameter(key::Symbol,
                   prior::Union{ContinuousUnivariateDistribution, ContinuousMultivariateDistribution};
                   fixed::Bool = false,
                   description::String = "No description available",
                   tex_label::String = "")
    val = length(prior) > 1 ? repeat([NaN], length(prior)) : NaN
    return parameter(key, val, (NaN, NaN), (NaN, NaN), Untransformed(), prior, fixed = fixed, scaling = identity, description = description, tex_label = tex_label)
end


"""
```
SteadyStateParameter(key::Symbol, value::T; description::String = "",
                      tex_label::String = "") where {T <: Number}
```

SteadyStateParameter constructor with optional `description` and `tex_label` arguments.
"""
function SteadyStateParameter(key::Symbol, value::T;
                              description::String = "No description available",
                              tex_label::String = "") where {T <: Number}
    return SteadyStateParameter(key, value, description, tex_label)
end


"""
```
parameter(p::UnscaledParameter{S,T,U}, newvalue::S) where {S<:Real,T<:Number,U<:Transform}
```

Returns an UnscaledParameter with value field equal to `newvalue`. If `p` is a fixed
parameter, it is returned unchanged.
"""
function parameter(p::UnscaledParameter{T,U}, newvalue::T) where {T <: Number, U <: Transform}
    p.fixed && return p    # if the parameter is fixed, don't change its value
    a,b = p.valuebounds
    if !(a <= newvalue <= b)
        throw(ParamBoundsError("New value of $(string(p.key)) ($(newvalue)) is out of bounds ($(p.valuebounds))"))
    end
    UnscaledParameter{T,U}(p.key, newvalue, p.valuebounds, p.transform_parameterization,
                           p.transform, p.prior, p.fixed, p.description, p.tex_label)
end

function parameter_ad(p::UnscaledParameterAD{S,T,U}, newvalue::Snew;
                   change_value_type::Bool = false) where {S<:Real, Snew<:Real, T <: Number, U <: Transform}
    p.fixed && return p    # if the parameter is fixed, don't change its value
    if !change_value_type && (typeof(p.value) != typeof(newvalue))
        error("Type of newvalue $(newvalue) does not match the type of the current value for parameter $(string(p.key)). Set keyword change_value_type = true if you want to overwrite the type of the parameter value.")
    end
    a,b = p.valuebounds
    if !(a <= newvalue <= b)
        throw(ParamBoundsError("New value of $(string(p.key)) ($(newvalue)) is out of bounds ($(p.valuebounds))"))
    end
    if change_value_type
        UnscaledParameterAD{Snew,T,U}(p.key, newvalue, p.valuebounds, p.transform_parameterization,
                                 p.transform, p.prior, p.fixed, p.description, p.tex_label)
    else
        UnscaledParameterAD{S,T,U}(p.key, newvalue, p.valuebounds, p.transform_parameterization,
                                 p.transform, p.prior, p.fixed, p.description, p.tex_label)
    end
end
=#

function parameter(p::UnscaledVectorParameter{V,T,U}, newvalue::V) where {V <: Vector, T <: Number, U <: Transform}
    p.fixed && return p    # if the parameter is fixed, don't change its value
    a,b = p.valuebounds
    if !all(a .<= newvalue .<= b)
        throw(ParamBoundsError("New value of $(string(p.key)) ($(newvalue)) is out of bounds ($(p.valuebounds))"))
    end
    UnscaledVectorParameter{V,T,U}(p.key, newvalue, p.valuebounds, p.transform_parameterization,
                           p.transform, p.prior, p.fixed, p.description, p.tex_label)
end


#="""
```
parameter(p::ScaledParameter{S,T,U}, newvalue::S) where {S<:Real, T<:Number,U<:Transform}
```

Returns a ScaledParameter with value field equal to `newvalue` and scaledvalue field equal
to `p.scaling(newvalue)`. If `p` is a fixed parameter, it is returned unchanged.
"""
function parameter(p::ScaledParameter{T,U}, newvalue::T) where {T <: Number, U <: Transform} #S:<Real, Snew:< Real
    p.fixed && return p    # if the parameter is fixed, don't change its value
    a,b = p.valuebounds
    if !(a <= newvalue <= b)
        throw(ParamBoundsError("New value of $(string(p.key)) ($(newvalue)) is out of bounds ($(p.valuebounds))"))
    end
    ScaledParameter{T,U}(p.key, newvalue, p.scaling(newvalue), p.valuebounds,
                         p.transform_parameterization, p.transform, p.prior, p.fixed,
                         p.scaling, p.description, p.tex_label)
end

function parameter_ad(p::ScaledParameterAD{S,T,U}, newvalue::Snew;
                      change_value_type::Bool = false) where {S<:Real, Snew<:Real, T<:Number, U<:Transform}
    p.fixed && return p    # if the parameter is fixed, don't change its value
    if !change_value_type && (typeof(p.value) != typeof(newvalue))
        error("Type of newvalue $(newvalue) does not match value of parameter $(string(p.key)).")
    end
    a,b = p.valuebounds
    if !(a <= newvalue <= b)
        throw(ParamBoundsError("New value of $(string(p.key)) ($(newvalue)) is out of bounds ($(p.valuebounds))"))
    end
    if change_value_type
        ScaledParameterAD{Snew,T,U}(p.key, newvalue, p.scaling(newvalue), p.valuebounds,
                               p.transform_parameterization, p.transform, p.prior, p.fixed,
                               p.scaling, p.description, p.tex_label)
    else
        ScaledParameterAD{S,T,U}(p.key, newvalue, p.scaling(newvalue), p.valuebounds,
                               p.transform_parameterization, p.transform, p.prior, p.fixed,
                               p.scaling, p.description, p.tex_label)
    end
end=#


function parameter(p::ScaledVectorParameter{V,T,U}, newvalue::V) where {V <: Vector, T <: Number, U <: Transform}
    p.fixed && return p    # if the parameter is fixed, don't change its value
    a,b = p.valuebounds
    if !all(a .<= newvalue .<= b)
        throw(ParamBoundsError("New value of $(string(p.key)) ($(newvalue)) is out of bounds ($(p.valuebounds))"))
    end
    ScaledVectorParameter{V,T,U}(p.key, newvalue, p.scaling.(newvalue), p.valuebounds,
                         p.transform_parameterization, p.transform, p.prior, p.fixed,
                         p.scaling, p.description, p.tex_label)
end
