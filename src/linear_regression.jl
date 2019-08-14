mutable struct LinearRegression{T} <: AbstractModel{T}
    parameters::ParameterVector{T}
    keys::Dict{Symbol, Int}
    spec::String
    subspec::String
    settings::Dict{Symbol, Setting}
    test_settings::Dict{Symbol, Setting}
    rng::MersenneTwister
    testing::Bool
end

decription(m::LinearRegression) = "You know what I am..."

function LinearRegression(spec::String="", subspec::String="ss0";
                          custom_settings::Dict{Symbol, Setting} = Dict{Symbol, Setting}(),
                          testing = false)

    settings           = Dict{Symbol,Setting}()
    test_settings      = Dict{Symbol,Setting}()
    rng                = MersenneTwister(0)

    m = LinearRegression{Float64}(
            Vector{AbstractParameter{Float64}}(), OrderedDict{Symbol, Int}(),
            spec,
            subspec,
            settings, test_settings,
            rng,
            testing)

    model_settings!(m)
    default_test_settings!(m)
    for custom_setting in values(custom_settings)
        m <= custom_setting
    end

    init_parameters!(m)

    return m
end


"""
```
init_parameters!(m::AnSchorfheide)
```

Initializes the model's parameters, as well as empty values for the steady-state
parameters (in preparation for `steadystate!(m)` being called to initialize
those).
"""
function init_parameters(m::LinearRegression)
    m <= parameter(:α, 1., Normal(0., 1.0), fixed = false)
    m <= parameter(:β, 1., Normal(0., 1.0), fixed = false)
end
