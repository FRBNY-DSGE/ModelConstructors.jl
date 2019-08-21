# ModelConstructors

```@meta
CurrentModule = ModelConstructors
```

The *ModelConstructors.jl* package implements a generic model object for estimation.
A model of any type can be defined as long as it has parameters.
Moreover, this package contains the building blocks of model objects used for both
Dynamic Stochastic General Equilibrium models
([DSGE.jl](https://github.com/FRBNY-DSGE/DSGE.jl)) and
Sequential Monte Carlo ([SMC.jl](https://github.com/FRBNY-DSGE/SMC.jl)).
The Parameter, Observable, Setting, State, etc. types are included.

## Table of Contents

```@contents
Pages = [
  "model_design.md",
  "example_model.md",
  "advanced_usage.md",
  "parameters.md",
  "settings.md",
  "observables.md"
]

## Acknowledgements

Developers of this package at the
[New York Fed](https://www.newyorkfed.org/research) include

* [William Chen](https://github.com/chenwilliam77)
* [Ethan Matlin](https://github.com/ethanmatlin)
* [Reca Sarfati](https://github.com/rsarfati)
