# Regime-Switching Parameters

```@meta
CurrentModule = ModelConstructors
```

Users may want certain parameters to regime-switch or otherwise change values over time
but maintain the same key rather than labeling parameters as, for example, `a1`, `a2`, etc.
We provide an interface that allows the user to, in principle, make nearly every feature
of a `Parameter` type regime-switching.

Every `Parameter` type we have implemented includes a `regimes` field,
which is a dictionary of dictionaries. The keys of `p.regimes`
are the names of the other fields of a parameter, e.g. `value`, and
`p.regimes[:value]` points to an `OrderedDict` mapping regime numbers
to values. Note that the types of the values of these `OrderedDict`s are not necessarily `Float64`.
For example, `p.regimes[:valuebounds]` will point to an `OrderedDict{Int, Tuple{S, S}} where {S <: Real}`.
Note that, because we use `OrderedDict`s, *the user must insert the parameter regimes
in a linear order*. We have not yet implemented a sorting function nor any checks
that regimes are sorted in the correct order.

After using functions we have written to easily update these dictionaries,
the user can toggle between different regimes using `toggle_regime!`.
Under the hood, `toggle_regime!` will overwrite the fields of
the parameter `p` with the values for regime `i` that are held
in `p.regimes`. For this reason, *users need to start with regime 1
when specifying regimes*.

## Regime-Switching Interface
```@autodocs
Modules = [ModelConstructors]
Pages   = ["regimes.jl"]
Order   = [:function]
```

## Tutorial
As a short example, suppose we wanted to implement a vector of regime-switching parameters.
Each parameter has 3 regimes, and they are fixed in regime 2 only. The following code snippet will
set up the parameter vector as desired. The comments provide additional information on functionality.

```
# Create initial parameter vector
pvec = ParameterVector{Float64}(undef, 2)
p1 = parameter(:p1, 1., (0., 5.), (0., 5.), ModelConstructors.SquareRoot(); fixed = false)
p2 = parameter(:p2, 1., (0., 10.), (0., 10.), ModelConstructors.SquareRoot(); fixed = false)

pvec[1] = p1
pvec[2] = p2

# Set regime values
set_regime_val!(p1, 1, 1.) # If you use fixed = true, then you'll need to add the kwarg `override_bounds = true`
set_regime_val!(p1, 2, 2.) # b/c fixed = true sets the `valuebounds` to a zero-length interval (e.g. Interval(1., 1.))
set_regime_val!(p1, 3, 3.) # so toggling b/n regimes will resort in valuebounds errors.

set_regime_val!(p2, 1, 3.) # Also note that you don't have to use the same values as the ones you used
set_regime_val!(p2, 2, 4.) # during instantiation when setting the regimes. Just make sure the regimes
set_regime_val!(p2, 3, 5.) # are added in linear order

# Specify which regimes are fixed
# Note that, under the hood, `set_regime_fixed!` will
# automatically implement regime-switching valuebounds
# b/c we fix parameters by checking the valuebounds
for p in pvec
    set_regime_fixed!(p, 1, false)
    set_regime_fixed!(p, 2, true)
    set_regime_fixed!(p, 3, false)
end

# Toggle regimes as desired!
# You can toggle the parameter vector directly
# or each parameter instance individually
toggle_regime!(pvec, 1)
```

Now, suppose that the notion of "parameter" regimes does not
necessarily coincide with the notion of "model" regimes.
As an example, in a linear state space model, the state transition or
measurement equations might change, even though the parameters remain
the same. In this case, we need some way of specifying the relationship
between model and parameter regimes.

To specify this relationship, we utilize an `AbstractDict{Int, Int}`
whose keys are the regimes of the model and values are the regimes
of the parameters. When toggling between regimes,
the user uses the syntax `toggle_regime!(p::Parameter, model_regime::Int, d::AbstractDict{Int, Int})`
instead of `toggle_regime!(p::Parameter, para_regime::Int)`. For example, suppose
that the model has 5 regimes, where regimes 1-3 match the parameter regimes,
and regimes 4-6 have the same parameters as regime 1.
We can implement such a specification as follows:

```
# Construct dictionary mapping model regimes to parameter regimes
model2para_regimes = Dict(1 => 1, 2 => 2, 3 => 3,
                          4 => 1, 5 => 1, 6 => 1)

# Toggle to parameter regime
# associated with model regime 3
toggle_regime!(pvec, 3, model2para_regimes)

# Now toggle to parameter regime
# associated with model regime 5
toggle_regime!(pvec, 5, model2para_regimes)
```
