# Creating Models

```@meta
CurrentModule = ModelConstructors
```

The ModelConstructors.jl provides several two models as well as example scripts
that show how to estimate a model.

- A generic model `GenericModel` designed as a template for user-defined models
  and for users who only needs functionality to estimate parameters.
- An example of a `LinearRegression` model with minimal fields.

To see examples of more complex models, we recommend looking at the source code
of models in [DSGE.jl](https://github.com/FRBNY-DSGE/DSGE.jl). These models
use most, if not all, of the fields of `GenericModel`.

## `GenericModel`

### Template for New Models

The idea of `GenericModel` is that it should have most fields required to define
a generic mathematical model. For most purposes, it will have more fields
than needed. If a user wants to create their own model but are not familiar
with the process of defining a new model type, then we recommend that the user copy
the source code of `GenericModel` into a new file and editing that file. Referencing
the source code of models in [DSGE.jl](https://github.com/FRBNY-DSGE/DSGE.jl)
will be useful.

### [Estimating CAPM](@id estim-capm)

Some users only need the ability to estimate parameters. `GenericModel` provides
a convenient way to do that since it includes functionality to choose settings
and observables. The script estimate_capm.jl in the examples/factor_model folder
in docs shows how to use `GenericModel` in this way. We can write

```julia
capm = GenericModel()
capm <= parameter(:α, 0., (-1e5, 1e5), (-1e5, 1e5), Untransformed(), Normal(0, 1e3),
                  fixed = false)
# add in more parameters...
```

This code instantiates a `GenericModel` object and adds a parameter called `α`, which
takes a default value of `0.` and is assigned a normal prior with mean zero and variance 1000.
The two `(-1e5, 1e5)` intervals specify the bounds of the parameter before and after
transformation. The `Untransformed()` call indicates that we do not transform the parameter,
hence why the bounds are the same. For more details on parameters,
see the [The `Parameter` type](@ref).

The example script then constructs a likelihood function. To use
[SMC.jl](https://github.com/FRBNY-DSGE/SMC.jl), the likelihood function
needs to take two arguments: parameters and data.
Using `GenericModel` makes this simple
because we just use `capm.parameters` as the parameters provided
as inputs to the likelihood function.

Finally, we just call `smc(likelihood_fnct, capm.parameters, data)` to estimate
the parameters of the CAPM model we have defined!

## `LinearRegression`
The `LinearRegression` type is an example of a model type that does not need to use
all the fields provided in `GenericModel` when estimating. For comparison,
the type definition for `GenericModel` is

```julia
mutable struct GenericModel{T} <: AbstractModel{T}
    parameters::ParameterVector{T}
    steady_state::ParameterVector{T}
    keys::Dict{Symbol, Int}
    endogenous_states::Dict{Symbol,Int}
    exogenous_shocks::Dict{Symbol,Int}
    observables::Dict{Symbol,Int}
    pseudo_observables::Dict{Symbol,Int}
    spec::String
    subspec::String
    settings::Dict{Symbol, Setting}
    test_settings::Dict{Symbol, Setting}
    rng::MersenneTwister
    testing::Bool
    observable_mappings::Dict{Symbol, Observable}
    pseudo_observable_mappings::Dict{Symbol, PseudoObservable}
```

while the type definition for `LinearRegression` is only

```julia
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
```
