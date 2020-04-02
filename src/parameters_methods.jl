import Base.length
import Base.iterate

function Base.show(io::IO, p::SteadyStateParameterArray{T}) where {T}
    @printf io "%s\n" typeof(p)
    @printf io "(:%s)\n%s\n"      p.key p.description
    @printf io "LaTeX label: %s\n"     p.tex_label
    @printf io "-----------------------------\n"
    @printf io "value:        [%+6f,...,%+6f]\n" p.value[1] p.value[end]
end

hasprior(p::Union{Parameter, ParameterAD, VectorParameter}) = !isnull(p.prior)

NullableOrPriorUnivariate   = Union{NullablePriorUnivariate,   ContinuousUnivariateDistribution}
NullableOrPriorMultivariate = Union{NullablePriorMultivariate, ContinuousMultivariateDistribution}

# We want to use value field from UnscaledParameters and
# SteadyStateParameters in computation, so we alias their union here.
UnscaledOrSteadyState = Union{UnscaledParameter, SteadyStateParameter}


function Base.show(io::IO, p::Parameter{T,U}) where {T, U} #S,T,U
    @printf io "%s\n" typeof(p)
    @printf io "(:%s)\n%s\n"      p.key p.description
    @printf io "LaTeX label: %s\n"     p.tex_label
    @printf io "-----------------------------\n"
    #@printf io "real value:        %+6f\n" transform_to_real_line(p)
    @printf io "unscaled, untransformed value:        %+6f\n" p.value
    isa(p,ScaledParameter) && @printf "scaled, untransformed value:        %+6f\n" p.scaledvalue
    #!isa(U(),Untransformed) && @printf io "transformed value: %+6f\n" p.value

    if hasprior(p)
        @printf io "prior distribution:\n\t%s\n" get(p.prior)
    else
        @printf io "prior distribution:\n\t%s\n" "no prior"
    end

    @printf io "transformation for csminwel:\n\t%s" U()
    @printf io "parameter is %s\n" p.fixed ? "fixed" : "not fixed"
end

function Base.show(io::IO, p::ParameterAD{S,T,U}) where {S,T,U}
    @printf io "%s\n" typeof(p)
    @printf io "(:%s)\n%s\n"      p.key p.description
    @printf io "LaTeX label: %s\n"     p.tex_label
    @printf io "-----------------------------\n"
    #@printf io "real value:        %+6f\n" transform_to_real_line(p)
    @printf io "unscaled, untransformed value:        %+6f\n" p.value
    isa(p,ScaledParameter) && @printf "scaled, untransformed value:        %+6f\n" p.scaledvalue
    #!isa(U(),Untransformed) && @printf io "transformed value: %+6f\n" p.value

    if hasprior(p)
        @printf io "prior distribution:\n\t%s\n" get(p.prior)
    else
        @printf io "prior distribution:\n\t%s\n" "no prior"
    end

    @printf io "transformation for csminwel:\n\t%s" U()
    @printf io "parameter is %s\n" p.fixed ? "fixed" : "not fixed"
end

function Base.show(io::IO, p::VectorParameter{V,T,U}) where {V,T, U}
    @printf io "%s\n" typeof(p)
    @printf io "(:%s)\n%s\n"      p.key p.description
    @printf io "LaTeX label: %s\n"     p.tex_label
    @printf io "-----------------------------\n"
    #@printf io "real value:        %+6f\n" transform_to_real_line(p)
   # join([@printf io "%+6f\n" x for x in p.value], ", ")
    #isa(p,ScaledParameter) && @printf "scaled, untransformed value:        %+6f\n" p.scaledvalue
    #!isa(U(),Untransformed) && @printf io "transformed value: %+6f\n" p.value

    if hasprior(p)
        @printf io "prior distribution:\n\t%s\n" get(p.prior)
    else
        @printf io "prior distribution:\n\t%s\n" "no prior"
    end
    @printf io "transformation for csminwel:\n\t%s" U()
    @printf io "parameter is %s\n" p.fixed ? "fixed" : "not fixed"
end


function Base.show(io::IO, p::SteadyStateParameter{T}) where {T}
    @printf io "%s\n"                 typeof(p)
    @printf io "(:%s)\n%s\n"          p.key p.description
    @printf io "LaTeX label: %s\n"    p.tex_label
    @printf io "-----------------------------\n"
    @printf io "value:        %+6f\n" p.value
end

function Base.show(io::IO, p::SteadyStateParameterGrid{T}) where {T}
    @printf io "%s\n"                      typeof(p)
    @printf io "(:%s)\n%s\n"               p.key p.description
    @printf io "LaTeX label: %s\n"         p.tex_label
    @printf io "-----------------------------\n"
    @printf io "value:        [%f,...,%f]" p.value[1] p.value[end]
end

"""
```
transform_to_model_space{S<:Real,T<:Number, U<:Transform}(p::Parameter{S,T,U}, x::S)
```

Transforms `x` from the real line to lie between `p.valuebounds` without updating `p.value`.
The transformations are defined as follows, where (a,b) = p.transform_parameterization and c
a scalar (default=1):

- Untransformed: `x`
- SquareRoot:    `(a+b)/2 + (b-a)/2 * c * x/sqrt(1 + c^2 * x^2)`
- Exponential:   `a + exp(c*(x-b))`
"""
transform_to_model_space(p::ParameterAD{S,<:Number,Untransformed}, x::S) where S = x
function transform_to_model_space(p::ParameterAD{S,<:Number,SquareRoot}, x::S) where S
    (a,b), c = p.transform_parameterization, one(S)
    (a+b)/2 + (b-a)/2*c*x/sqrt(1 + c^2 * x^2)
end

transform_to_model_space(p::Parameter{T,Untransformed}, x::T) where T = x
function transform_to_model_space(p::Parameter{T,SquareRoot}, x::T) where T
    (a,b), c = p.transform_parameterization, one(T)
    (a+b)/2 + (b-a)/2*c*x/sqrt(1 + c^2 * x^2)
end
function transform_to_model_space(p::ParameterAD{S,<:Number,Exponential}, x::S) where S
    (a,b), c = p.transform_parameterization, one(S)
    a + exp(c*(x-b))
end

function transform_to_model_space(p::Parameter{T,Exponential}, x::T) where T
    (a,b), c = p.transform_parameterization, one(T)
    a + exp(c*(x-b))
end

transform_to_model_space(pvec::ParameterVector{T}, values::Vector{T}) where T = map(transform_to_model_space, pvec, values)
transform_to_model_space(pvec::ParameterVector, values::Vector{S}) where S = map(transform_to_model_space, pvec, values)

"""
```
differentiate_transform_to_model_space{S<:Real,T<:Number, U<:Transform}(p::Parameter{S,T,U}, x::S)
```

Differentiates the transform of `x` from the real line to lie between `p.valuebounds`
The transformations are defined as follows, where (a,b) = p.transform_parameterization and c
a scalar (default=1):

- Untransformed: `x`
- SquareRoot:    `(a+b)/2 + (b-a)/2 * c * x/sqrt(1 + c^2 * x^2)`
- Exponential:   `a + exp(c*(x-b))`

Their gradients are therefore

- Untransformed: `1`
- SquareRoot:    `(b-a)/2 * c / (1 + c^2 * x^2)^(3/2)`
- Exponential:   `c * exp(c*(x-b))`
"""
differentiate_transform_to_model_space(p::ParameterAD{S,<:Number,Untransformed}, x::S) where S = one(S)
function differentiate_transform_to_model_space(p::ParameterAD{S,<:Number,SquareRoot}, x::S) where S
    (a,b), c = p.transform_parameterization, one(S)
    (b-a)/2 * c / (1 + c^2 * x^2)^(3/2)
end
function differentiate_transform_to_model_space(p::ParameterAD{S,<:Number,Exponential}, x::S) where S
    (a,b), c = p.transform_parameterization, one(S)
    c * exp(c*(x-b))
end
differentiate_transform_to_model_space(pvec::ParameterVector, values::Vector{S}) where S = map(differentiate_transform_to_model_space, pvec, values)

"""
```
transform_to_real_line(p::Parameter{S,T,U}, x::S = p.value) where {S<:Real, T<:Number, U<:Transform}
```

Transforms `p.value` from model space (between `p.valuebounds`) to the real line, without updating
`p.value`. The transformations are defined as follows,
where (a,b) = p.transform_parameterization, c a scalar (default=1), and x = p.value:

- Untransformed: x
- SquareRoot:   (1/c)*cx/sqrt(1 - cx^2), where cx =  2 * (x - (a+b)/2)/(b-a)
- Exponential:   b + (1 / c) * log(x-a)
"""
transform_to_real_line(p::ParameterAD{S,<:Number,Untransformed}, x::S = p.value) where S = x
function transform_to_real_line(p::ParameterAD{S,<:Number,SquareRoot}, x::S = p.value) where S
    (a,b), c = p.transform_parameterization, one(S)
    cx = 2. * (x - (a+b)/2.)/(b-a)
    if cx^2 >1
        println("Parameter is: $(p.key)")
        println("a is $a")
        println("b is $b")
        println("x is $x")
        println("cx is $cx")
        error("invalid paramter value")
    end
    (1/c)*cx/sqrt(1 - cx^2)
end
function transform_to_real_line(p::ParameterAD{S,<:Number,Exponential}, x::S = p.value) where S
    (a,b),c = p.transform_parameterization,one(S)
    b + (1 ./ c) * log(x-a)
end

transform_to_real_line(pvec::ParameterVector, values::Vector{S}) where S  = map(transform_to_real_line, pvec, values)
transform_to_real_line(pvec::ParameterVector{S}) where S = map(transform_to_real_line, pvec)

transform_to_real_line(p::Parameter{T,Untransformed}, x::T = p.value) where T = x
function transform_to_real_line(p::Parameter{T,SquareRoot}, x::T = p.value) where T
    (a,b), c = p.transform_parameterization, one(T)
    cx = 2. * (x - (a+b)/2.)/(b-a)
    if cx^2 >1
        println("Parameter is: $(p.key)")
        println("a is $a")
        println("b is $b")
        println("x is $x")
        println("cx is $cx")
        error("invalid paramter value")
    end
    (1/c)*cx/sqrt(1 - cx^2)
end
function transform_to_real_line(p::Parameter{T,Exponential}, x::T = p.value) where T
    (a,b),c = p.transform_parameterization,one(T)
    b + (1 ./ c) * log(x-a)
end

transform_to_real_line(pvec::ParameterVector{T}, values::Vector{T}) where T  = map(transform_to_real_line, pvec, values)
transform_to_real_line(pvec::ParameterVector{T}) where T = map(transform_to_real_line, pvec)

"""
```
differentiate_transform_to_real_line{S<:Real,T<:Number, U<:Transform}(p::Parameter{S,T,U}, x::S)
```

Differentiates the transform of `x` from the model space lying between `p.valuebounds` to the real line.
The transformations are defined as follows, where (a,b) = p.transform_parameterization and c
a scalar (default=1):

- Untransformed: x
- SquareRoot:   (1/c)*cx/sqrt(1 - cx^2), where cx =  2 * (x - (a+b)/2)/(b-a)
- Exponential:   b + (1 / c) * log(x-a)

Their gradients are therefore

- Untransformed: `1`
- SquareRoot:    `(1/c) * (1 / ( 1 - cx^2)^(-3/2)) * (2/(b-a))`
- Exponential:   `1 / (c * (x - a))`
"""
differentiate_transform_to_real_line(p::ParameterAD{S,<:Number,Untransformed}, x::S) where S = one(S)
function differentiate_transform_to_real_line(p::ParameterAD{S,<:Number,SquareRoot}, x::S) where S
    (a,b), c = p.transform_parameterization, one(S)
    cx = 2 * (x - (a+b)/2)/(b-a)
    (1/c) * (1 / (1 - cx^2)^(-3/2)) * (2/(b-a))
end
function differentiate_transform_to_real_line(p::ParameterAD{S,<:Number,Exponential}, x::S) where S
    (a,b), c = p.transform_parameterization, one(S)
    1 / (c * (x - a))
end
differentiate_transform_to_real_line(pvec::ParameterVector, values::Vector{S}) where S = map(differentiate_transform_to_real_line, pvec, values)
differentiate_transform_to_real_line(pvec::ParameterVector{S}) where S = map(differentiate_transform_to_real_line, pvec)

# define operators to work on parameters

Base.convert(::Type{T}, p::UnscaledParameter) where {T<:Real}     = convert(T,p.value)
Base.convert(::Type{T}, p::UnscaledVectorParameter) where {T <: Vector}     = convert(T,p.value)
Base.convert(::Type{T}, p::ScaledParameter) where {T<:Real}       = convert(T,p.scaledvalue)
Base.convert(::Type{T}, p::ScaledVectorParameter) where {T <: Vector}       = convert(T,p.scaledvalue)

Base.convert(::Type{T}, p::SteadyStateParameter) where {T<:Real}  = convert(T,p.value)
Base.convert(::Type{ForwardDiff.Dual{T,V,N}}, p::UnscaledParameter) where {T,V,N} = convert(V,p.value)
Base.convert(::Type{ForwardDiff.Dual{T,V,N}}, p::ScaledParameter) where {T,V,N} = convert(V,p.scaledvalue)

Base.promote_rule(::Type{AbstractParameter{T}}, ::Type{U}) where {T<:Real, U<:Number} = promote_rule(T,U)
Base.promote_rule(::Type{AbstractVectorParameter{T}}, ::Type{U}) where {T<:Vector, U<:Vector} = promote_rule(T,U)

Base.length(p::UnscaledVectorParameter) = length(p.value)
Base.length(p::ScaledVectorParameter) = length(p.value)
Base.iterate(p::UnscaledVectorParameter) = iterate(p.value)
Base.iterate(p::ScaledVectorParameter) = iterate(p.value)

# Define scalar operators on parameters
for op in (:(Base.:+),
           :(Base.:-),
           :(Base.:*),
           :(Base.:/),
           :(Base.:^))

    # For regular parameter
    @eval ($op)(p::UnscaledOrSteadyState, q::UnscaledOrSteadyState) = ($op)(p.value, q.value)
    @eval ($op)(p::UnscaledOrSteadyState, x::Integer)               = ($op)(p.value, x)
    @eval ($op)(p::UnscaledOrSteadyState, x::Number)                = ($op)(p.value, x)
    @eval ($op)(x::Number, p::UnscaledOrSteadyState)                = ($op)(x, p.value)

    @eval ($op)(p::ScaledParameter, q::ScaledParameter) = ($op)(p.scaledvalue, q.scaledvalue)
    @eval ($op)(p::ScaledParameter, x::Integer)         = ($op)(p.scaledvalue, x)
    @eval ($op)(p::ScaledParameter, x::Number)          = ($op)(p.scaledvalue, x)
    @eval ($op)(x::Number, p::ScaledParameter)          = ($op)(x, p.scaledvalue)

    @eval ($op)(p::ScaledParameter, q::UnscaledOrSteadyState) = ($op)(p.scaledvalue, q.value)
    @eval ($op)(p::UnscaledOrSteadyState, q::ScaledParameter) = ($op)(p.value, q.scaledvalue)


    # For Vector Parameter
    @eval ($op)(p::UnscaledVectorParameter, q::UnscaledVectorParameter) = ($op)(p.value, q.value)
    @eval ($op)(p::UnscaledVectorParameter, x::Integer)          = ($op).(p.value, x)
    @eval ($op)(x::Integer, p::UnscaledVectorParameter)          = ($op).(p.value, x)

    @eval ($op)(p::UnscaledVectorParameter, x::Number)           = ($op).(p.value, x)
    @eval ($op)(x::Number, p::UnscaledVectorParameter)           = ($op).(p.value, x)

    @eval ($op)(p::ScaledVectorParameter, q::ScaledVectorParameter) = ($op)(p.scaledvalue, q.scaledvalue)
    @eval ($op)(p::ScaledVectorParameter, x::Integer)          = ($op).(p.scaledvalue, x)
    @eval ($op)(x::Integer, p::ScaledVectorParameter)          = ($op).(p.scaledvalue, x)
    @eval ($op)(p::ScaledVectorParameter, x::Number)           = ($op).(p.scaledvalue, x)
    @eval ($op)(x::Number, p::ScaledVectorParameter)           = ($op).(x, p.scaledvalue)

    @eval ($op)(p::ScaledVectorParameter, q::UnscaledOrSteadyState) = ($op)(p.scaledvalue, q.value)
end

# Define scalar functional mappings and comparisons
for f in (:(Base.exp),
          :(Base.log),
          :(Base.transpose),
          :(Base.:-),
          :(Base.:<),
          :(Base.:>),
          :(Base.:<=),
          :(Base.:>=))

    @eval ($f)(p::UnscaledOrSteadyState) = ($f)(p.value)
    @eval ($f)(p::ScaledParameter) = ($f)(p.scaledvalue)

    if f != :(Base.:-)
        @eval ($f)(p::UnscaledOrSteadyState, x::Number) = ($f)(p.value, x)
        @eval ($f)(p::ScaledParameter, x::Number) = ($f)(p.scaledvalue, x)
        @eval ($f)(p::ScaledVectorParameter, x::Number)          = ($f).(p.scaledvalue, x)
        @eval ($f)(x::Number, p::ScaledVectorParameter)          = ($f).(p.scaledvalue, x)
        @eval ($f)(p::UnscaledVectorParameter, x::Number)          = ($f).(p.scaledvalue, x)
        @eval ($f)(x::Number, p::UnscaledVectorParameter)          = ($f).(p.scaledvalue, x)
    end
end

# Define scalar operators on grids
for op in (:(Base.:+),
           :(Base.:-),
           :(Base.:*),
           :(Base.:/))

    @eval ($op)(g::SteadyStateParameterGrid, x::Integer)        = ($op)(g.value, x)
    @eval ($op)(g::SteadyStateParameterGrid, x::Number)         = ($op)(g.value, x)
    @eval ($op)(x::Integer, g::SteadyStateParameterGrid)        = ($op)(x, g.value)
    @eval ($op)(x::Number, g::SteadyStateParameterGrid)         = ($op)(x, g.value)
end

# Overload length, iterate to broadcast works properly
Base.length(p::Parameter) = length(p.value)
Base.iterate(p::Parameter, state=1) = state > length(p.value) ? nothing : (state, state+1)

"""
```
update!(pvec::ParameterVector, values::Vector{S}; change_value_type::Bool = false) where S
```

Update all parameters in `pvec` that are not fixed with
`values`. Length of `values` must equal length of `pvec`.
Function optimized for speed.
"""
function update!(pvec::ParameterVector, values::Vector{T};
                 change_value_type::Bool = false) where T
    # this function is optimised for speed
    @assert length(values) == length(pvec) "Length of input vector (=$(length(values))) must match length of parameter vector (=$(length(pvec)))"
    if change_value_type
        tmp = if typeof(pvec[1]) <: ParameterAD
            (x,y) -> parameter_ad(x, y; change_value_type = change_value_type)
        else
            (x,y) -> parameter(x, y; change_value_type = change_value_type)
        end
        map!(tmp, pvec, pvec, values)
    else
        if typeof(pvec[1]) <: ParameterAD
            map!(parameter_ad, pvec, pvec, values)
        else
            map!(parameter, pvec, pvec, values)
        end
    end
end

"""
```
update!(pvec::ParameterVector, values::Vector{S},
    indices::BitArray{1}; change_value_type::Bool = false) where S
```

Updates a subset of parameters in `pvec` specified by indices. Assumes `values`
is sorted in the same order as the parameters in `pvec`, ignoring parameters
that are to be left unchanged.

However, `update!` will not overwrite fixed parameters, even if
`indices` has a true in an index corresponding to a fixed parameter.


# Examples
```jldoctest
julia> pvec = ParameterVector{Float64}(undef, 3);
julia> pvec[1] = parameter(:a, 1., (0., 3.), (0., 3.), fixed = false);
julia> pvec[2] = parameter(:b, 1.);
julia> pvec[3] = parameter(:c, 1., (0., 3.), (0., 3.), fixed = false);
julia> values = [2., 2.];
julia> update!(pvec, values, [true, false, true]);
julia> map(x -> x.value, pvec)
3-element Array{Float64,1}:
 2.0
 1.0
 2.0

```

```jldoctest
julia> pvec = ParameterVector{Float64}(undef, 3);
julia> pvec[1] = parameter(:a, 1.);
julia> pvec[2] = parameter(:b, 1.);
julia> pvec[3] = parameter(:c, 1.);
julia> values = [2., 2.];
julia> update!(pvec, values, [true, false, true]);
julia> map(x -> x.value, pvec)
3-element Array{Float64,1}:
 1.0
 1.0
 1.0

```
"""
function update!(pvec::ParameterVector, values::AbstractVector{S}, indices::BitArray{1};
                 change_value_type::Bool = false) where S

    # Are you changing all indices? Or a subset?
    if all(indices)
        current_vals = values
    else
        @assert count(indices) == length(values) "Length of input vector (=$(length(values))) must match number of values in parameter vector that `indices` specified to be changed (=$(count(indices)))"
        # Infer whether we need to declare current_vals as a Vector{<:Real}
        # (e.g. if values are ForwardDiff.Dual types)
        # or if current_vals can be a smaller memory type (e.g. Float64)
        try
            eltype(values)::AbstractFloat
            current_vals = Vector{eltype(values)}(map(x -> x.value, pvec))
        catch
            current_vals = Vector{Real}(map(x -> x.value, pvec))
        end
        current_vals[indices] .= values
    end

    update!(pvec, current_vals; change_value_type = change_value_type)
end

"""
```
update(pvec::ParameterVector, values::Vector{S}) where S
```

Returns a copy of `pvec` where non-fixed parameter values are updated
to `values`. `pvec` remains unchanged. Length of `values` must
equal length of `pvec`.

We define the non-mutating version like this because we need the type stability of map!
"""
update(pvec::ParameterVector, values::Vector{S}) where S = update!(copy(pvec), values)

Distributions.pdf(p::AbstractParameter) = exp(logpdf(p))
# we want the unscaled value for ScaledParameters
Distributions.logpdf(p::Parameter{T,U}) where {T, U} = logpdf(get(p.prior),p.value)

# this function is optimised for speed
function Distributions.logpdf(pvec::ParameterVector{T}) where T
	x = zero(T)
	@inbounds for i = 1:length(pvec)
        if hasprior(pvec[i])
    		x += logpdf(pvec[i])
        end
	end
	x
end

# calculate logpdf at new values, without needing to allocate a temporary array with update
function Distributions.logpdf(pvec::ParameterVector, values::Vector{S}) where S
    @assert length(values) == length(pvec) "Length of input vector (=$(length(values))) must match length of parameter vector (=$(length(pvec)))"

    x = zero(S)
    @inbounds for i = 1:length(pvec)
        if hasprior(pvec[i])
            x += logpdf(parameter(pvec[i], values[i]))
        end
    end
    x
end

Distributions.pdf(pvec::ParameterVector{T}) where T  = exp(logpdf(pvec))
Distributions.pdf(pvec::ParameterVector, values::Vector{S}) where S = exp(logpdf(pvec, values))

"""
```
Distributions.rand(p::Vector{AbstractParameter{Float64}})
```

Generate a draw from the prior of each parameter in `p`.
"""
function Distributions.rand(p::Vector{AbstractParameter{Float64}}) #Distributions.rand(p::Vector{AbstractParameter{Float64}})
    draw = zeros(length(p))
    for (i, para) in enumerate(p)
        draw[i] = if para.fixed
            para.value
        else
            # Resample until all prior draws are within the value bounds
            prio = rand(para.prior.value)
            while !(para.valuebounds[1] < prio < para.valuebounds[2])
                prio = rand(para.prior.value)
            end
            prio
        end
    end
    return draw
end

function Distributions.rand(p::Vector{AbstractVectorParameter{Vector{Float64}, Float64}})
    draw = zeros(length(p), 2)
    for (i, para) in enumerate(p)
        draw[i, :] = if para.fixed
            para.value
        else
            # Resample until all prior draws are within the value bounds
            prio = rand(para.prior.value)
            while !((para.valuebounds[1] < prio[1] < para.valuebounds[2]) &&
                    (para.valuebounds[1] < prio[2] < para.valuebounds[2]))
                prio = rand(para.prior.value)
            end
            prio
        end
    end
    @show draw
    return draw
end


"""
```
Distributions.rand(p::Vector{AbstractParameter{Float64}}, n::Int)
```

Generate `n` draws from the priors of each parameter in `p`.This returns a matrix of size
`(length(p),n)`, where each column is a sample.
"""
function Distributions.rand(p::Vector{AbstractParameter{Float64}}, n::Int) #Distributions.rand(p::Vector{AbstractParameter{Float64}}, n::Int)
    priorsim = zeros(length(p), n)
    for i in 1:n
        priorsim[:, i] = rand(p)
    end
    return priorsim
end



"""
```
moments(θ::Parameter)
```

If θ's prior is a `RootInverseGamma`, τ and ν. Otherwise, returns the mean
and standard deviation of the prior. If θ is fixed, returns `(θ.value, 0.0)`.
"""
function moments(θ::Parameter)
    if θ.fixed
        return θ.value, 0.0
    else
        prior = get(θ.prior)
        if isa(prior, RootInverseGamma)
            return prior.τ, prior.ν
        else
            return mean(prior), std(prior)
        end
    end
end

function describe_prior(param::Parameter)
    if param.fixed
        return "fixed at " * string(param.value)

    elseif !param.fixed && !isnull(param.prior)
        (prior_mean, prior_std) = moments(param)

        prior_dist = string(typeof(get(param.prior)))
        prior_dist = replace(prior_dist, "Distributions." => "")
        prior_dist = replace(prior_dist, "DSGE." => "")
        prior_dist = replace(prior_dist, "{Float64}" => "")

        mom1, mom2 = if isa(prior, RootInverseGamma)
            "tau", "nu"
        else
            "mu", "sigma"
        end

        return prior_dist * "(" * mom1 * "=" * string(round(prior_mean, digits=4)) * ", " *
                                  mom2 * "=" * string(round(prior_std, digits=4)) * ")"

    else
        error("Parameter must either be fixed or have non-null prior: " * string(param.key))
    end
end
