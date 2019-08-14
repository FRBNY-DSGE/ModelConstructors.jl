mutable struct GenericModel{T} <: AbstractModel{T}
    parameters::ParameterVector{T}
    steady_state::ParameterVector{T}
    keys::Dict{Symbol, Int}
    endogenous_states::Dict{Symbol,Int}             # these fields used to create matrices in the
    exogenous_shocks::Dict{Symbol,Int}              # measurement and equilibrium condition equations.
    observables::Dict{Symbol,Int}                   #
    pseudo_observables::Dict{Symbol,Int}            #
    spec::String
    subspec::String
    settings::Dict{Symbol, Setting}
    test_settings::Dict{Symbol, Setting}
    rng::MersenneTwister
    testing::Bool
    observable_mappings::Dict{Symbol, Observable}
    pseudo_observable_mappings::Dict{Symbol, PseudoObservable}
end

function GenericModel(spec::String = "", subspec::String = ""; custom_settings::Dict{Symbol, Setting} = Dict{Symbol, Setting}(), testing = false)
    settings = Dict{Symbol, Setting}()
    test_settings = Dict{Symbol, Setting}()
    rng = MersenneTwister(0)

    m = GenericModel{Float64}(
        Vector{AbstractParameter{Float64}}(), Vector{AbstractParameter{Float64}}(),
        Dict{Symbol, Int}(),
        Dict{Symbol, Int}(), Dict{Symbol, Int}(), Dict{Symbol, Int}(), Dict{Symbol, Int}(),
        spec, subspec, settings, test_settings, rng, testing,
        Dict{Symbol, Observable}(), Dict{Symbol, PseudoObservable}())

    model_settings!(m)
    default_test_settings!(m)
    for custom_setting in values(custom_settings)
        m <= custom_setting
    end

    return m
end


description(m::GenericModel) = "A generic model: You can put parameters, states, shocks, observables, pseudo-observables, spec, subpsec, settings inside of me"


function model_settings!(m::GenericModel)

end
