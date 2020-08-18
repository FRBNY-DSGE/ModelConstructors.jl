abstract type AbstractModel{T} end

function Base.show(io::IO, m::AbstractModel)
    @printf io "Model \n"
    @printf io "no. parameters:             %i\n" n_parameters(m)
    @printf io "description:\n %s\n"          description(m)
end

@inline function Base.getindex(m::AbstractModel, i::Integer)
    if i <= (j = length(m.parameters))
        return m.parameters[i]
    else
        return m.steady_state[i-j]
    end
end

# Need to define like this so as to disable bounds checking
@inline function Base.getindex(m::AbstractModel, k::Symbol)
    i = m.keys[k]
    @inbounds if i <= (j = length(m.parameters))
        return m.parameters[i]
    else
        return m.steady_state[i-j]
    end
end

@inline function Base.setindex!(m::AbstractModel, value::Number, i::Integer)
    if i <= (j = length(m.parameters))
        param = m.parameters[i]
        param.value = value
        if isa(param, ScaledParameter)
            param.scaledvalue = param.scaling(value)
        end
        return param
    else
        steady_state_param = m.steady_state[i-j]
        steady_state_param.value = value
        return steady_state_param
    end
end

Base.setindex!(m::AbstractModel, value::Array, k::Symbol) = Base.setindex!(m, value, m.keys[k])

@inline function Base.setindex!(m::AbstractModel, value::Array, i::Integer)
    if i <= (j = length(m.parameters))
        param = m.parameters[i]
        param.value = value
        if isa(param, ScaledParameter)
            param.scaledvalue = param.scaling(value)
        end
        return param
    else
        steady_state_param = m.steady_state[i-j]
        steady_state_param.value = value
        return steady_state_param
    end
end

"""
```
setindex!(m::AbstractModel, param::AbstractParameter, i::Integer)
```

If `i`<length(m.parameters), overwrites m.parameters[i] with
param. Otherwise, overwrites m.steady_state[i-length(m.parameters).
"""
@inline function Base.setindex!(m::AbstractModel, param::AbstractParameter, i::Integer)
    if i <= (j = length(m.parameters))
        m.parameters[i] = param
    else
        m.steady_state[i-j] = param
    end
    return param
end

Base.setindex!(m::AbstractModel, value, k::Symbol) = Base.setindex!(m, value, m.keys[k])

"""
```
(<=)(m::AbstractModel{T}, p::AbstractParameter{T}) where T
```

Syntax for adding a parameter to a model: m <= parameter.
NOTE: If `p` is added to `m` and length(m.steady_state) > 0, `keys(m)` will not generate the
index of `p` in `m.parameters`.
"""
function (<=)(m::AbstractModel{T}, p::AbstractParameter{T}) where T

    if !in(p.key, keys(m.keys))

        new_param_index = length(m.keys) + 1

        # grow parameters and add the parameter
        push!(m.parameters, p)

        # add parameter location to dict
        setindex!(m.keys, new_param_index, p.key)
    else
        # overwrite the previous parameter with the new one
        setindex!(m, p, p.key)
    end
end


"""
```
(<=)(m::AbstractModel{T}, ssp::Union{SteadyStateParameter,SteadyStateParameterArray}) where {T}
```

Add a new steady-state value to the model by appending `ssp` to the `m.steady_state` and
adding `ssp.key` to `m.keys`.
"""
function (<=)(m::AbstractModel{T}, ssp::Union{SteadyStateParameter, SteadyStateParameterArray}) where {T}

    if !in(ssp.key, keys(m.keys))
        new_param_index = length(m.keys) + 1

        # append ssp to steady_state vector
        push!(m.steady_state, ssp)

        # add parameter location to dict
        setindex!(m.keys, new_param_index, ssp.key)
    else
        # overwrite the previous parameter with the new one
        setindex!(m, ssp, ssp.key)
    end
end

"""
```
(<=)(m::AbstractModel{T}, ssp::SteadyStateParameterGrid) where {T}

```

Add a new steady-state value to the model by appending `ssp` to the `m.steady_state` and
adding `ssp.key` to `m.keys`.
"""
function (<=)(m::AbstractModel{T}, ssp::SteadyStateParameterGrid) where {T}

    if !in(ssp.key, keys(m.keys))
        new_param_index = length(m.keys) + 1

        # append ssp to steady_state vector
        push!(m.steady_state, ssp)

        # add parameter location to dict
        setindex!(m.keys, new_param_index, ssp.key)
    else
        # overwrite the previous parameter with the new one
        setindex!(m, ssp, ssp.key)
    end
end

Distributions.logpdf(m::AbstractModel) = logpdf(m.parameters)
Distributions.pdf(m::AbstractModel) = exp(logpdf(m))

# Convenience functions
n_states(m::AbstractModel)                  = length(m.endogenous_states)
n_states_augmented(m::AbstractModel)        = n_states(m) + length(m.endogenous_states_augmented)
n_shocks_exogenous(m::AbstractModel)        = length(m.exogenous_shocks)
n_shocks_expectational(m::AbstractModel)    = length(m.expected_shocks)
n_observables(m::AbstractModel)             = length(m.observables)
n_pseudo_observables(m::AbstractModel)      = length(m.pseudo_observables)
n_equilibrium_conditions(m::AbstractModel)  = length(m.equilibrium_conditions)
n_parameters(m::AbstractModel)              = length(m.parameters)
n_parameters_steady_state(m::AbstractModel) = length(m.steady_state)
n_parameters_free(m::AbstractModel)         = sum([!α.fixed for α in m.parameters])

function n_parameters_regime_switching(m::AbstractModel)
    return n_parameters_regime_switching(m.parameters)
end

"""
```
get_dict(m, class, index)
```
"""
function get_dict(m::AbstractModel, class::Symbol)
    if class == :states
        m.endogenous_states
    elseif class == :obs
        m.observables
    elseif class == :pseudo
        m.pseudo_observables
    elseif class in [:shocks, :stdshocks]
        m.exogenous_shocks
    else
        throw(ArgumentError("Invalid class: $class. Must be :states, :obs, :pseudo, :shocks, or :stdshocks"))
    end
end

"""
```
get_key(m, class, index)
```

Returns the name of the state (`class = :states`), observable (`:obs`),
pseudo-observable (`:pseudo`), or shock (`:shocks` or `:stdshocks`)
corresponding to the given `index`.
"""
function get_key(m::AbstractModel, class::Symbol, index::Int)
    dict = get_dict(m, class)
    out = Base.filter(key -> dict[key] == index, collect(keys(dict)))
    if length(out) == 0
        error("Key corresponding to index $index not found for class: $class")
    elseif length(out) > 1
        error("Multiple keys corresponding to index $index found for class: $class")
    else
        return out[1]
    end
end


# Interface for I/O settings
spec(m::AbstractModel)         = m.spec
subspec(m::AbstractModel)      = m.subspec
saveroot(m::AbstractModel)     = get_setting(m, :saveroot)
dataroot(m::AbstractModel)     = get_setting(m, :dataroot)

# Interface for data
data_vintage(m::AbstractModel) = get_setting(m, :data_vintage)
data_id(m::AbstractModel)      = get_setting(m, :data_id)

#=
Build paths to where input/output/results data are stored.

Description:
Creates the proper directory structure for input and output files, treating the DSGE/save
    directory as the root of a savepath directory subtree. Specifically, the following
    structure is implemented:

    dataroot/

    savepathroot/
                 output_data/<spec>/<subspec>/log/
                 output_data/<spec>/<subspec>/<out_type>/raw/
                 output_data/<spec>/<subspec>/<out_type>/work/
                 output_data/<spec>/<subspec>/<out_type>/tables/
                 output_data/<spec>/<subspec>/<out_type>/figures/

Note: we refer to the savepathroot/output_data/<spec>/<subspec>/ directory as saveroot.
=#
# """
# ```
# logpath(model)
# ```
# Returns path to log file. Path built as
# ```
# <output root>/output_data/<spec>/<subspec>/log/log_<filestring>.log
# ```
# """
# function logpath(m::AbstractModel)
#     return savepath(m, "log", "log.log")
# end

strs = [:work, :raw, :tables, :figures, :log]
fns = [Symbol(x, "path") for x in strs]
for (str, fn) in zip(strs, fns)
    @eval begin
        # First eval function
        function $fn(m::AbstractModel,
                     out_type::String,
                     file_name::String = "",
                     filestring_addl::Vector{String}=Vector{String}())
            return savepath(m, out_type, $(string(str)), file_name, filestring_addl)
        end

        # Then, add docstring to it
        @doc $(
        """
        ```
        $fn(m::AbstractModel, out_type::String, file_name::String="",
            filestring_addl::Vector{String}=Vector{String}())
        ```

        Returns path to specific $str output file, creating containing directory as needed. If
        `file_name` not specified, creates and returns path to containing directory only.
        Path built as
        ```
        <output root>/output_data/<spec>/<subspec>/<out_type>/$str/<file_name>_<filestring>.<ext>
        ```
        """
        ) $fn
    end
end

# Not exposed to user. Actually create path and insert model string to file name.
function savepath(m::AbstractModel,
                  out_type::String,
                  sub_type::String,
                  file_name::String = "",
                  filestring_addl::Vector{String} = Vector{String}())
    # Containing directory
    dir = String(joinpath(saveroot(m), "output_data", spec(m), subspec(m), out_type, sub_type))

    if !isempty(file_name)
        base = filestring_base(m)
        return savepath(dir, file_name, base, filestring_addl)
    else
        return dir
    end
end

function savepath(dir::String,
                  file_name::String = "",
                  filestring_base::Vector{String} = Vector{String}(),
                  filestring_addl::Vector{String} = Vector{String}())
    if !isdir(dir)
        mkpath(dir)
    end

    if !isempty(file_name)
        (base, ext) = splitext(file_name)
        myfilestring = filestring(filestring_base, filestring_addl)
        file_name_detail = base * myfilestring * ext

        return joinpath(dir, file_name_detail)
    else
        return dir
    end
end


# Input data handled slightly differently, because it is not model-specific.
"""
```
inpath(m::AbstractModel, in_type::T, file_name::T="") where T<:String
```

Returns path to specific input data file, creating containing directory as needed. If
`file_name` not specified, creates and returns path to containing directory only. Valid
`in_type` includes:

* `\"raw\"`: raw input series
* `\"data\"`: transformed data in model units
* `\"cond\"`: conditional data - nowcasts for the current forecast quarter, or related
* `\"user\"`: user-supplied data for starting parameter vector, hessian, or related
* `\"scenarios\"`: alternative scenarios

Path built as
```
<data root>/<in_type>/<file_name>
```
"""
function inpath(m::AbstractModel, in_type::String, file_name::String="")
    path = dataroot(m)
    # Normal cases.
    if in_type in ["raw", "data", "cond", "scenarios"]
        path = joinpath(path, in_type)
    # User-provided inputs. May treat this differently in the future.
    elseif in_type == "user"
        path = joinpath(path, "user")
    else
        error("Invalid in_type: ", in_type)
    end

    # Containing dir
    if !isdir(path)
        mkpath(path)
    end

    # If file_name provided, return full path
    if !isempty(file_name)
        path = joinpath(path, file_name)
    end

    return path
end

function filestring_base(m::AbstractModel)
    if !m.testing
        base = Vector{String}()
        for (skey, sval) in m.settings
            if sval.print
                push!(base, to_filestring(sval))
            end
        end
        return base
    else
        return ["test"]
    end
end

filestring(m::AbstractModel) = filestring(m, Vector{String}(undef, 0))
filestring(m::AbstractModel, d::String) = filestring(m, [String(d)])
function filestring(m::AbstractModel, d::Vector{String})
    base = filestring_base(m)
    return filestring(base, d)
end

function filestring(base::Vector{String}, d::Vector{String})
    filestrings = vcat(base, d)
    sort!(filestrings)
    return "_" * join(filestrings, "_")
end

function filestring(d::Vector{String})
    sort!(d)
    return "_" * join(d, "_")
end

"""
```
rand(d::Union{DegenerateMvNormal,MvNormal}, m::AbstractModel; cc::AbstractFloat = 1.0)
```

Generate a draw from `d` with variance optionally scaled by `cc^2`.
"""
function rand(d::Union{DegenerateMvNormal,MvNormal}, m::AbstractModel; cc::AbstractFloat = 1.0)
    return d.μ + cc*d.σ*randn(m.rng, length(d))
end

"""
```
rand(d::Union{DegenerateMvNormal,MvNormal}, rng::MersenneTwister; cc::AbstractFloat = 1.0)
```

Generate a draw from `d` with variance optionally scaled by `cc^2`.
"""
function rand(d::Union{DegenerateMvNormal,MvNormal}, rng::MersenneTwister; cc::AbstractFloat = 1.0)
    return d.μ + cc*d.σ*randn(rng, length(d))
end

"""
`rand_prior(m::AbstractModel; ndraws::Int = 100_000)`

Draw a random sample from the model's prior distribution.
"""
function rand_prior(m::AbstractModel; ndraws::Int = 100_000)
    T = typeof(m.parameters[1].value)
    npara = length(m.parameters)
    priorsim = Array{T}(undef, ndraws, npara)

    for i in 1:ndraws
        priodraw = Array{T}(undef, npara)

        # Parameter draws per particle
        for j in 1:length(m.parameters)

            priodraw[j] = if !m.parameters[j].fixed
                prio = rand(m.parameters[j].prior.value)

                # Resample until all prior draws are within the value bounds
                while !(m.parameters[j].valuebounds[1] < prio < m.parameters[j].valuebounds[2])
                    prio = rand(m.parameters[j].prior.value)
                end

                prio
            else
                m.parameters[j].value
            end
        end
        priorsim[i,:] = priodraw'
    end

    priorsim
end

@inline function unpack(m::AbstractModel{S}, ::Val{k}) where {S <: Real, k}
    isa(m[k], ScaledParameter) ? m[k].scaledvalue::S : m[k].value::S
end
