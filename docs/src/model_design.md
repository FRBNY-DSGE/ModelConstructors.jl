# Model Design

*ModelConstructors.jl* is an object-oriented approach to
working with mathematical models. This approach
takes advantage of Julia's type system, multiple dispatch, package-handling
mechanism, and other features. A single model object centralizes
 all information about the model's parameters, states, equilibrium conditions, and
settings in a single data structure. The model object also keeps track of file locations
for all I/O operations.

The most minimal model only needs to have parameters, but a generic model may
have one or more of the following objects:

- **Parameters**: Have values, bounds, fixed-or-not status, priors, and regime switching. An
  instance of the `AbstractParameter` type houses all information about a given
  parameter in a single data structure. See
  [The `AbstractParameter` Type](@ref).
- **Settings**: Provide a general way to choose settings that affect how a model
  is manipulated. See [The `Setting` Type](@ref).
- **Observables and Pseudo-Observables**: Mapping of names to indices, as well as
  information necessary for transformations. See
  [The `Observable` and `PseudoObservable` Types](@ref).

These are enough to define a generic model. *Everything else* is essentially
a function of these basics. For example, estimating a model requires two more steps:
defining likelihood function and selecting an estimation routine.
See [Estimating CAPM](@ref estim-capm) for an example estimation using SMC.
