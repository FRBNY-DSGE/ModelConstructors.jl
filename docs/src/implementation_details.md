# Implementation Details

```@meta
CurrentModule = ModelConstructors
```

This section describes important functions and implementation features in
greater detail. Additional documentation
can also be found in function documentation or in-line.

This section focuses on what the code does and why. Docstrings and the code itself
(including comments) provide detailed information regarding *how* these basic
procedures are implemented.

## The `AbstractModel` Type

The `AbstractModel` type provides a common interface for all model objects,
which greatly facilitates the implementation of new model specifications. Any
concrete subtype of `AbstractModel` can be passed to any function defined for
`AbstractModel`, provided that the concrete type has the fields that the
function expects to be available.

If a user wants to define a new
subclass of models, say regression models, then the user could
create a new `AbstractRegressionModel` type as a subtype of `AbstractModel`.
Functions defined for `AbstractRegressionModel` would only
apply to concrete subtypes of `AbstractRegressionModel`, but
functions defined for `AbstractModel` will still work on these
concrete subtypes.


## The `AbstractParameter` Type

The `AbstractParameter` type implements our notion of a model parameter: a
time-invariant, unobserved value that has significance in the model, e.g.
for likelihood computation and estimation.

Though all parameters are time-invariant, they can have different features.
Some parameters are scaled for use when solving the model[^1] and
constructing the model's measurement equations[^2].
**Currently, you cannot use broadcasting on
any concrete subtype of `AbstractParameter`**

During optimization, parameters may be transformed from
model space to the real line via one of three different transformations: `Untransformed`,
`SquareRoot`, and `Exponential`. These
transformations are also defined as types, and require additional information
for each parameter. Typically, we have two "hyperparameters" for
these transformations, `a`, and `b`.
- `Untransformed`: `a` and `b` do nothing
- `SquareRoot`: `a` and `b` specify the bounds the parameter takes, i.e. ``x\in (a, b)``
- `Exponential`: `a` and `b` are the parameters in the transformation ``a + exp(x - b)``

In some models, steady state values might be relevant parameters.
They are typically functions of other parameters, so they do not need
to be estimated directly.

While parameters are "time-invariant", we do allow regime switching.
As an example, suppose that we have a linear regression with data
from time periods $t = 1,\dots, T$, where $T > 4$, and in $t = 3$,
the intercept of the regression is assumed to change values because of
a structural break in the time series. We can
model the intercept as a parameter with regime-switching. The parameter
has one value in periods $t = 1, 2$ and a different value in periods
$t = 3,\dots, T$. Currently, only regime-switching in the values
of the parameter has been tested, but we have implemented
regime switching in all the features. For example, you may
want a different prior in each regime. See [Regime-Switching Interface](@ref) for
documentation on the interface for regime-switching parameters.

The various requirements on parameters are nicely addressed using a parameterized type
hierarchy.

- `AbstractParameter{T<:Number}`: The common abstract supertype for all
  parameters.
    - `Parameter{T<:Number, U<:Transform}`: The abstract supertype for
      parameters that are directly estimated.
        - `UnscaledParameter{T<:Number, U:<Transform}`: Concrete type for
          parameters that do not need to be scaled for equilibrium conditions.
        - `ScaledParameter{T<:Number, U:<Transform}`: Concrete type for
          parameters that are scaled for equilibrium conditions.
    - `SteadyStateParameter{T<:Number}`: Concrete type for steady-state
      parameters.

All `Parameter`s have the fields defined in `UnscaledParameter`:

```@docs
UnscaledParameter
```

`ScaledParameters` also have the following fields:

- `scaledvalue::T`: Parameter value scaled for use in `eqcond.jl`
- `scaling::Function`: Function used to scale parameter value for use in
  equilibrium conditions.

*Note:* Though not strictly necessary, defining a scaling with the parameter
object allows for much a much cleaner definition of the equilibrium conditions.

Because the values of `SteadyStateParameter`s are directly computed as a
function of `ScaledParameter`s and `UnscaledParameter`s, they only require 4
fields:

```@docs
SteadyStateParameter
```


## The `Observable` and `PseudoObservable` Types

We similarly encapsulate information about observables and pseudo-observables
(unobserved linear combinations of states, e.g. the output gap) into the
`Observable` and `PseudoObservable` types. Each type has identifier fields
`key`, `name`, and `longname`.

Most importantly, both `Observable`s and `PseudoObservable`s include the
information needed for transformations to and from model units. For
`Observable`s, these are the `input_series`, `fwd_transform`, and
`rev_transform` fields. "Forward transformations" are applied to transform
the raw input data series specified in `input_series` to model units. The
model is estimated and forecasted in model units, and then we apply "reverse
transformations" to get human-readable units before computing means and bands or
plotting. Pseudo-observables are not observed, so they do not have
`input_series` or `fwd_transform`s, but they may however have `rev_transform`s.

As an example, the `:obs_gdp` `Observable` uses as `input_series` aggregate
nominal GDP in levels, the GDP price index, and population in levels, all from
FRED.[^3] These series are `fwd_transform`ed to get quarter-over-quarter log growth
rates of per-capita real GDP, which are the `Observable`'s model units. The
reverse transformation then converts `:obs_gdp` into annualized
quarter-over-quarter percent changes of *aggregate* real GDP.


```@docs
Observable
PseudoObservable
```

## Model Settings

The `Setting` type implements computational settings that affect how the code
runs without affecting the mathematical definition of the model. Depending on the model,
these may include flags (e.g. whether or not to recompute the Hessian),
parameterization for the Metropolis-Hastings algorithm (e.g. number of times
to draw from the posterior distribution), and the vintage of data being used
(`Setting` is a parametric type - a `Setting{T<:Any}`, so Booleans, Numbers,
and Strings can all be turned into `Setting`s). If settings exist for
a model type, then they should be stored centrally in
the `settings` dictionary within the model object.

Why implement a `Setting` type when we could put their values directly into the
source code or dictionary? The most obvious answer is that the parametric type
allows us to implement a single interface for all `Setting`s (Booleans, Strings,
etc.), so that when we access a particular setting during the estimation and
forecast steps, we don't have to think about the setting's type.

`Setting`s play an important role in addition to providing useful abstraction.
Estimating and forecasting the New York Fed DSGE model takes many hours of
computation time and creates a lot of output files. It is useful to be able to
compare model output from two different models whose settings differ slightly
(for example, consider two identical models that use different vintages of data
as input). A central feature of the `Setting` type is a mechanism that generates
unique, meaningful filenames when code is executed with different settings.
Specifically, when a setting takes on a non-default value, a user-defined
setting code (along with the setting's value) are appended to all output files
generated during execution.

The `Setting{T<:Any}` type is defined as follows:

```@docs
Setting
```

We provide two functions `default_settings!` and `default_test_settings!`
to initialize settings that most models can have. The settings are

- save root
- input data root
- vintage of data to be used
- dataset id

To update the value of an existing function, the user has two
options. First, the user may use the `<=` syntax. However, for this
to work properly, it is essential that the setting's `key` field be
exactly the same as that of an existing entry in
`m.settings`. Otherwise, an additional entry will be added to
`m.settings` and the old setting will be the one accessed from other
all routines. A potentially safer, though clunkier, option is to use the [`update!`](@ref) method.


## Type Interfaces

### `Parameter` Interface

```@autodocs
Modules = [ModelConstructors]
Pages = ["parameters.jl"]
Order = [:function]
```

### Regime-Switching Interface
To implement regime-switching, we add a field to `Parameter` types
called `regimes::Dict{Symbol, OrderedDict{Int, Any}}`. The keys
of the top level dictionary are the names of the other fields
in a `Parameter` type, e.g. `:value`. Each key then points to
an `OrderedDict`, whose keys are the numbers of different regimes
and values are the corresponding values for each regime.

The field `regimes` functions as a "storage" of information.
When a `Parameter` type interacts with another object in Julia,
e.g. `p + 1.`, where `p` is a `Parameter`, what actually happens is
`p.value + 1.`. Only the current fields of `p` will be used
when interacting with other objects. To use a different value
(or different fields) from another regime, the user needs to
tell the parameter to switch regimes the `toggle_regime!` function (see below).

By default, the `regimes` field is empty (see the documentation
of the `parameter` function in [`Parameter` Interface](@ref)).
To add values, either pass in the dictionary as a keyword
to `parameter` or use `set_regime_val!`. Note that the latter
function is not exported.

*Note that regimes must be sorted in order* because we
store the regimes as an `OrderedDict`, and `OrderedDict`
objects are sorted by insertion order.

```@autodocs
Modules = [ModelConstructors]
Pages = ["regimes.jl"]
Order = [:function]
```

### `Setting` Interface

```@autodocs
Modules = [ModelConstructors]
Pages = ["settings.jl"]
Order = [:function]
```

    rng::MersenneTwister
    testing::Bool
    observable_mappings::Dict{Symbol, Observable}
    pseudo_observable_mappings::Dict{Symbol, PseudoObservable}



[^1]: By solving the model, we mean a mapping from parameters to
           some objects of interest. In a state space model,
           solving the model is a mapping from parameters
           to a state transition function. By constructing

[^2]: In the context of a state space model,
            a measurement equation is mapping from states to observable data.

[^3]: In [DSGE.jl](https://github.com/FRBNY-DSGE/DSGE.jl), we implement a
             `load_data` function that parses `input_series` to retrieve data
             from FRED. To take full advantage of the `Observable` type, users may
             want to write their own `load_data` function. For example, it may
             be convenient to write a `load_data` function that parses `input_series`
             to select column(s) from saved CSV files and combines them into
             a single data frame.
