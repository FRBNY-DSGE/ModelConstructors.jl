import Base: <=

Interval{T} = Tuple{T,T}
"""
```
Transform
```

Subtypes of the abstract Transform type indicate how a `Parameter`'s
value is transformed from model space (which can be bounded to a
limited section of the real line) to the entire real line (which is
necessary for mode-finding using csminwel). The transformation is
performed by the `transform_to_real_line` function, and is reversed by the
`transform_to_model_space` function.
"""
abstract type Transform end

struct Untransformed <: Transform end
struct SquareRoot    <: Transform end
struct Exponential   <: Transform end

Base.show(io::IO, t::Untransformed) = @printf io "x -> x\n"
Base.show(io::IO, t::SquareRoot)    = @printf io "x -> (a+b)/2 + (b-a)/2*c*x/sqrt(1 + c^2 * x^2)\n"
Base.show(io::IO, t::Exponential)   = @printf io "x -> b + (1/c) * log(x-a)\n"

"""
```
AbstractParameter{T<:Number}
```

The AbstractParameter type is the common supertype of all model
parameters, including steady-state values.  Its subtype structure is
as follows:

-`AbstractParameter{T<:Number}`: The common abstract supertype for all parameters.
    -`Parameter{S<:Real,T<:Number, U<:Transform}`: The abstract supertype for parameters that are directly estimated.
        -`UnscaledParameter{S<:Real, T<:Number, U:<Transform}`: Concrete type for parameters that do not need to be scaled for equilibrium conditions.
        -`ScaledParameter{S<:Real, T<:Number, U:<Transform}`: Concrete type for parameters that are scaled for equilibrium conditions.
    -`SteadyStateParameter{T<:Number}`: Concrete type for steady-state parameters.
"""
abstract type AbstractParameter{T<:Number} end

"""
```
Parameter{ S<:Real, T<:Number, U<:Transform} <: AbstractParameter{T}
```

The Parameter type is the common supertype of time-invariant, non-steady-state model
parameters. It has 2 subtypes, `UnscaledParameter` and `ScaledParameter`.
`ScaledParameter`s are parameters whose values are scaled when used in the model's
equilibrium conditions. The scaled value is stored for convenience, and updated when the
parameter's value is updated.

The Parameter type has separate `T` and `S` types to allow for
automatic differentiation, which will make the type of `S` a Dual type. By specifying
this difference, while we cannot enforce `T` and `S` to always sensibly match
each others types, we can avoid the issue of having to recast the types of fields
with type `T` to be Duals as well.
"""
abstract type Parameter{T,U<:Transform} <: AbstractParameter{T} end
abstract type ParameterAD{S<:Real,T,U} <: Parameter{T,U} end

# ParameterVector is a wrapper for a vector
# that takes any subtype of AbstractParameter, but it is not
# an "abstract" type since we are not intending
# to define subtypes of ParameterVector.
ParameterVector{T}         =  Vector{AbstractParameter{T}}

NullablePriorUnivariate    =  Nullables.Nullable{ContinuousUnivariateDistribution}
NullablePriorMultivariate  =  Nullables.Nullable{ContinuousMultivariateDistribution}

"""
```
UnscaledParameter{S<:Real,T<:Number,U<:Transform} <: Parameter{S,T,U}
```

Time-invariant model parameter whose value is used as-is in the model's equilibrium
conditions.

#### Fields
- `key::Symbol`: Parameter name. For maximum clarity, `key`
  should conform to the guidelines established in the DSGE Style Guide.
- `value::S`: Parameter value. Initialized in model space (guaranteed
  to be between `valuebounds`), but can be transformed between model
  space and the real line via calls to `transform_to_real_line` and
`transform_to_model_space`.
- `valuebounds::Interval{T}`: Bounds for the parameter's value in model space.
- `transform_parameterization::Interval{T}`: Parameters used to
  transform `value` between model space and the real line.
- `transform::U`: Transformation used to transform `value` between
  model space and real line.
- `prior::NullablePrior`: Prior distribution for parameter value.
- `fixed::Bool`: Indicates whether the parameter's value is fixed rather than estimated.
- `description::String`:  A short description of the parameter's economic
  significance.
- `tex_label::String`: String for printing the parameter name to LaTeX.
"""
mutable struct UnscaledParameterAD{S,T,U} <: ParameterAD{S,T,U} # New parameter type for Autodiff
    key::Symbol
    value::S                                # parameter value in model space
    valuebounds::Interval{T}                # bounds of parameter value
    transform_parameterization::Interval{T} # parameters for transformation
    transform::U                            # transformation between model space and real line for optimization
    prior::NullablePriorUnivariate          # prior distribution
    fixed::Bool                             # is this parameter fixed at some value?
    description::String
    tex_label::String                       # LaTeX label for printing
end

mutable struct UnscaledParameter{T,U} <: Parameter{T,U} # Old parameter type
    key::Symbol
    value::T                                # parameter value in model space
    valuebounds::Interval{T}                # bounds of parameter value
    transform_parameterization::Interval{T} # parameters for transformation
    transform::U                            # transformation between model space and real line for optimization
    prior::NullablePriorUnivariate          # prior distribution
    fixed::Bool                             # is this parameter fixed at some value?
    description::String
    tex_label::String                       # LaTeX label for printing
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
mutable struct ScaledParameterAD{S,T,U} <: ParameterAD{S,T,U}
    key::Symbol
    value::S
    scaledvalue::S
    valuebounds::Interval{T}
    transform_parameterization::Interval{T}
    transform::U
    prior::NullablePriorUnivariate
    fixed::Bool
    scaling::Function
    description::String
    tex_label::String
end

mutable struct ScaledParameter{T,U} <: Parameter{T,U}
    key::Symbol
    value::T #S
    scaledvalue::T #S
    valuebounds::Interval{T}
    transform_parameterization::Interval{T}
    transform::U
    prior::NullablePriorUnivariate
    fixed::Bool
    scaling::Function
    description::String
    tex_label::String
end


"""
```
SteadyStateParameter{T} <: AbstractParameter{T}
```

Steady-state model parameter whose value depends upon the value of other (non-steady-state)
`Parameter`s. `SteadyStateParameter`s must be constructed and added to an instance of a
model object `m` after all other model `Parameter`s have been defined. Once added to `m`,
`SteadyStateParameter`s are stored in `m.steady_state`. Their values are calculated and set
by `steadystate!(m)`, rather than being estimated directly. `SteadyStateParameter`s do not
require transformations from the model space to the real line or scalings for use in
equilibrium conditions.

#### Fields

- `key::Symbol`: Parameter name. Should conform to the guidelines
  established in the DSGE Style Guide.
- `value::T`: The parameter's steady-state value.
- `description::String`: Short description of the parameter's economic significance.
- `tex_label::String`: String for printing parameter name to LaTeX.
"""
mutable struct SteadyStateParameter{T} <: AbstractParameter{T}
    key::Symbol
    value::T
    description::String
    tex_label::String
end

"""
```
SteadyStateValueGrid{T} <: AbstractParameter{T}
```

Steady-state model parameter grid (for heterogeneous agent models) whose value is calculated by an
iterative procedure.
`SteadyStateParameterGrid`s must be constructed and added to an instance of a
model object `m` after all other model `Parameter`s have been defined. Once added to `m`,
`SteadyStateParameterGrid`s are stored in `m.steady_state`. Their values are calculated and set
by `steadystate!(m)`, rather than being estimated directly. `SteadyStateParameter`s do not
require transformations from the model space to the real line or scalings for use in
equilibrium conditions.

#### Fields

- `key::Symbol`: Parameter name. Should conform to the guidelines
  established in the DSGE Style Guide.
- `value::Array{T}`: The parameter's steady-state value grid.
- `description::String`: Short description of the parameter's economic significance.
- `tex_label::String`: String for printing parameter name to LaTeX.
"""
mutable struct SteadyStateParameterGrid{T} <: AbstractParameter{T}
    key::Symbol
    value::Array{T}
    description::String
    tex_label::String
end

function SteadyStateParameterGrid(key::Symbol,
                                  value::Array{T};
                                  description::String = "No description available",
                                  tex_label::String = "") where {T<:Number}

    return SteadyStateParameterGrid{T}(key, value, description, tex_label)
end

"""
```
SteadyStateParameterArray{T} <: AbstractParameter{T}
```
Steady-state model parameter whose value is an Array and
 depends upon the value of other (non-steady-state)
`Parameter`s. `SteadyStateParameterArray`s must be constructed and added to an instance of a
model object `m` after all other model `Parameter`s have been defined. Once added to `m`,
`SteadyStateParameterArray`s are stored in `m.steady_state`. Their values are calculated and set
by `steadystate!(m)`, rather than being estimated directly. `SteadyStateParameterArray`s do not
require transformations from the model space to the real line or scalings for use in
equilibrium conditions.

#### Fields

- `key::Symbol`: Parameter name. Should conform to the guidelines
  established in the DSGE Style Guide.
- `value::Array{T}`: The parameter's steady-state values.
- `description::String`: Short description of the parameter's economic significance.
- `tex_label::String`: String for printing parameter name to LaTeX.
"""
mutable struct SteadyStateParameterArray{T} <: AbstractParameter{T}
    key::Symbol
    value::Array{T}
    description::String
    tex_label::String
end

"""
```
SteadyStateParameterArray{T<:Number}(key::Symbol, value::Array{T};
                                description::String = "",
                                tex_label::String = "")
```

SteadyStateParameter constructor with optional `description` and `tex_label` arguments.
"""
function SteadyStateParameterArray(key::Symbol,
                                   value::Array{T};
                                   description::String = "No description available",
                                   tex_label::String = "") where {T<:Number}

    return SteadyStateParameterArray(key, value, description, tex_label)
end

NullableOrPriorUnivariate   = Union{NullablePriorUnivariate,   ContinuousUnivariateDistribution}
NullableOrPriorMultivariate = Union{NullablePriorMultivariate, ContinuousMultivariateDistribution}

# We want to use value field from UnscaledParameters and
# SteadyStateParameters in computation, so we alias their union here.
UnscaledOrSteadyState = Union{UnscaledParameter, SteadyStateParameter}

"""
```
ParamBoundsError <: Exception
```

A `ParamBoundsError` is thrown upon an attempt to assign a parameter value that is not
between `valuebounds`.
"""
mutable struct ParamBoundsError <: Exception
    msg::String
end
ParamBoundsError() = ParamBoundsError("Value not between valuebounds")
Base.showerror(io::IO, ex::ParamBoundsError) = print(io, ex.msg)


function parameter(key::Symbol,
                   value::Union{T, V}, #value::Union{S,V},
                   valuebounds::Interval{T} = (value,value),
                   transform_parameterization::Interval{T} = (value,value),
                   transform::U             = Untransformed(),
                   prior::Union{NullableOrPriorUnivariate, NullableOrPriorMultivariate} = NullablePriorUnivariate();
                   fixed::Bool              = true,
                   scaling::Function        = identity,
                   description::String = "No description available.",
                   tex_label::String = "") where {V<:Vector, T <: Float64, U <:Transform} #{V<:Vector, S<:Real, T <: Float64, U <:Transform}

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



"""
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
end
