# ModelConstructors.jl v0.2.5
- Fix bug in `get_values` when using regime-switching parameters

# ModelConstructors.jl v0.2.4
- Extend `transform_to_model_space` and `transform_to_real_line` for regime-switching parameters.

# ModelConstructors.jl v0.2.3
- Move methods for computing free and fixed indices of parameters from SMC.jl to ModelConstructors.jl

# ModelConstructors.jl v0.2.2
- Try to convert types to match rather than throwing a `MethodError` immediately
  when calling `parameter`.

# ModelConstructors.jl v0.2.1
- Raise compat bounds for some packages

# ModelConstructors.jl v0.2.0
- Restrict Julia version to at least 1.0
- Add @unpack macro for accessing parameters of a model
- Extend regime-switching to allow switching in `prior`, `fixed`, and `valuebounds` fields.
- Allow difference between the notion of "model regimes" and "parameter regimes" by specifying a dictionary which maps model regimes to parameter regimes.

# ModelConstructors.jl v0.1.12 Release Notes
- Update compatibility bounds
- Fix bugs in examples

# ModelConstructors.jl v0.1.11 Release Notes
- Fix bugs in v0.1.10

# ModelConstructors.jl v0.1.10 Release Notes
- Add random sampling with regime switching in Parameter types

# ModelConstructors.jl v0.1.9 Release Notes
- Implement regime switching in Parameter types.

# ModelConstructors.jl v0.1.8 Release Notes
- Add back New Parameter types that work with both Autodiff functionality and allow backwards compatibility

# ModelConstructors.jl v0.1.7 Release Notes
- Temporarily revert back to old Parameter types to fix SMC.jl tests and allow full backwards compatibility to old Parameters

# ModelConstructors.jl v0.1.6 Release Notes
- Resolve package compatibility issues, so as to be backwards-compatible with Julia 0.7.

# ModelConstructors.jl v0.1.5 Release Notes
Update tests
- Generalize to permit Real types (e.g. allows integration for ForwardDiff)
- Gradients of parameter transformations
- Fix package loading for v1.1
- Add function to initialize DegenerateMvNormal object (needed in the DSGE.jl package)

# ModelConstructors.jl v0.1.4 Release Notes
Update tests
Bug fixes and cleanup

# ModelConstructors.jl v0.1.3 Release Notes
Bug fixes and cleanup

# ModelConstructors.jl v0.1.2 Release Notes
Bug fixes and cleanup

# ModelConstructors.jl v0.1.1 Release Notes
Bug fixes and cleanup
