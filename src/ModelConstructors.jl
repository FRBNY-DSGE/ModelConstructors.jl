isdefined(Base, :__precompile__) && __precompile__()

module ModelConstructors

    using DataFrames, Dates, Distributed, Distributions, Nullables, Printf, Random
    using SpecialFunctions
    using LinearAlgebra

    import Base.isempty, Base.<, Base.min, Base.max
    export
        # distributions_ext.jl
        BetaAlt, GammaAlt, RootInverseGamma, DegenerateMvNormal, DegenerateDiagMvTDist,
        MatrixNormal, <=,

        # settings.jl
        Setting, get_setting,

        # observables.jl
        Observable, PseudoObservable, check_mnemonics,

        # defaults.jl
        default_settings!, default_test_settings!,

        # abstractmodel.jl
        AbstractModel, description,
        n_states, n_states_augmented, n_shocks_exogenous, n_shocks_expectational,
        n_equilibrium_conditions, n_observables, n_parameters, n_parameters_steady_states,
        n_parameters_free, n_pseudo_observables, get_dict, get_key,
        spec, subspec, saveroot, dataroot,
        data_vintage, data_id, cond_vintage,
        logpath, workpath, rawpath, tablespath, figurespath, inpath, filestring_base, workpath,
        rawpath, tablespath, figurespath, logpath, savepath, inpath, filestring,

        # parameters.jl
        parameter, Transform, NullablePrior, AbstractParameter,
        Parameter, ParameterVector, ScaledParameter,
        UnscaledParameter, SteadyStateParameter, transform_to_real_line, transform_to_model_space,
        update, update!, transform_to_model_space, transform_to_real_line, Interval,
        ParamBoundsError, Untransformed, SquareRoot, moments,
        prior,

        # distributions_ext.jl
        Uniform, Exponential, Normal, BetaAlt, GammaAlt, RootInverseGamma, pdf, logpdf, rand,
        DegenerateMvNormal, rank, length, DgenerateDiagMvTDist, mean, std,
        MatrixNormal, size, params,

        AbstractVectorParameter, VectorParameter, VectorParameterVector, ScaledVectorParameter,
        UnscaledVectorParameter, Untransformed,

        # statistics.jl
        prior, posterior, posterior!,

        # util.jl
        info_print, warn_print, println, print, @test_matrix_approx_eq, @test_matrix_eq2,

        # Generic Model
        GenericModel,

        # Linear Regression
        LinearRegression

    	const VERBOSITY = Dict(:none => 0, :low => 1, :high => 2)

    include("parameters.jl")
    include("distributions_ext.jl")
    include("abstractmodel.jl")
    include("settings.jl")
    include("observables.jl")
    include("defaults.jl")
    include("statistics.jl")
    include("util.jl")
    include("generic_model.jl")
    include("linear_regression.jl")
end
