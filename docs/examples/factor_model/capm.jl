mutable struct CAPM{T} <: AbstractModel{T}
    parameters::ParameterVector{T}
    keys::OrderedDict{Symbol,Int}
    spec::String
    subspec::String
    settings::Dict{Symbol,Setting}
    testing::Bool
    rng::MersenneTwister
end

function CAPM(subspec::String="ss0")
    spec               = split(basename(@__FILE__),'.')[1]
    subspec            = subspec
    settings           = Dict{Symbol,Setting}()
    rng                = MersenneTwister(0)

    m = CAPM{Float64}(Vector{AbstractParameter{Float64}}(), OrderedDict{Symbol,Int}(),
                               spec, subspec, settings, false, rng)

    # Set settings
    model_settings!(m)

    # Initialize parameters
    init_parameters!(m)

    # Initialize subspec
    init_subspec!(m)

    return m
end

function model_settings!(m::CAPM)
    default_settings!(m)

    # Data
    fn = dirname(@__FILE__)
    m <= Setting(:dataroot, "$(fn)/../save/input_data/")
    m <= Setting(:saveroot, "$(fn)/../save/")
end

function init_parameters!(m::CAPM)
    m <= parameter(:α1, 0., (-1e5, 1e5), (-1e5, 1e5), DSGE.Untransformed(), Normal(0, 1e3),
                   fixed = false)
    m <= parameter(:β1, 0., (-1e5, 1e5), (-1e5, 1e5), DSGE.Untransformed(), Normal(0, 1e3),
                   fixed = false)
    m <= parameter(:σ1, 1., (1e-5, 1e5), (1e-5, 1e5), DSGE.SquareRoot(), Uniform(0, 1e3),
                   fixed = false)
    m <= parameter(:α2, 0., (-1e5, 1e5), (-1e5, 1e5), DSGE.Untransformed(), Normal(0, 1e3),
                   fixed = false)
    m <= parameter(:β2, 0., (-1e5, 1e5), (-1e5, 1e5), DSGE.Untransformed(), Normal(0, 1e3),
                   fixed = false)
    m <= parameter(:σ2, 1., (1e-5, 1e5), (1e-5, 1e5), DSGE.SquareRoot(), Uniform(0, 1e3),
                   fixed = false)
    m <= parameter(:α3, 0., (-1e5, 1e5), (-1e5, 1e5), DSGE.Untransformed(), Normal(0, 1e3),
                   fixed = false)
    m <= parameter(:β3, 0., (-1e5, 1e5), (-1e5, 1e5), DSGE.Untransformed(), Normal(0, 1e3),
                   fixed = false)
    m <= parameter(:σ3, 1., (1e-5, 1e5), (1e-5, 1e5), DSGE.SquareRoot(), Uniform(0, 1e3),
                   fixed = false)
end

function init_subspec!(m::CAPM)
    if subspec(m) == "ss0"
        return
    else
        error("This subspec should be ss0")
    end
end
