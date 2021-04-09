using Test, ModelConstructors, Distributions, Dates, Nullables, Random

@testset "Ensure transformations to the real line/model space are valid" begin
    for T in subtypes(Transform)
        global u = parameter(:σ_pist, 2.5230, (1e-8, 5.), (1e-8, 5.), T(), fixed=false)
        @test ( transform_to_real_line(u) |> x -> transform_to_model_space(u,x) ) == u.value

        if !isa(T,Type{ModelConstructors.Untransformed})
            # check transform_to_real_line and transform_to_model_space to different things if T is not ModelConstructors.Untransformed
            @test transform_to_real_line(u,u.value) != transform_to_model_space(u,u.value)
        end
    end
end

tomodel_answers = zeros(3)
toreal_answers  = zeros(3)
a = 1e-8; b = 5.; c = 1.
x = 2.5230
cx = 2 * (x - (a+b)/2)/(b-a)
toreal_answers[1] = 1. / (c * (x - a))
toreal_answers[2] = (1/c) * (1. / (1. - cx^2)^(-3/2)) * (2/(b-a))
toreal_answers[3] = 1.
x = transform_to_real_line(parameter(:σ_pist, 2.5230, (1e-8, 5.), (1e-8, 5.),
                                     ModelConstructors.Exponential(), fixed=false))
tomodel_answers[1] = c * exp(c * (x - b))
x = transform_to_real_line(parameter(:σ_pist, 2.5230, (1e-8, 5.), (1e-8, 5.),
                                     ModelConstructors.SquareRoot(), fixed=false))
tomodel_answers[2] = (b - a) / 2 * c / (1 + c^2 * x^2)^(3/2)
tomodel_answers[3] = 1.
@testset "Ensure derivatives of transformations to the real line/model space are valid" begin
    for (i,T) in enumerate(subtypes(Transform))
        global u = parameter_ad(:σ_pist, 2.5230, (1e-8, 5.), (1e-8, 5.), T(), fixed=false)
        @test differentiate_transform_to_real_line(u, u.value) == toreal_answers[i]
        global x = transform_to_real_line(u)
        @test differentiate_transform_to_model_space(u,x) == tomodel_answers[i]

        if !isa(T,Type{ModelConstructors.Untransformed})
            # check transform_to_real_line and transform_to_model_space to different things if T is not ModelConstructors.Untransformed
            @test differentiate_transform_to_real_line(u,u.value) != differentiate_transform_to_model_space(u,u.value)
        end
    end
end

@testset "Check type conversion for `value`, `valuebounds`, and `transform_parameterization`" begin
    @info "The following warning is expected"
    u1 = parameter(:σ_pist, 2.5230, (1, 5), (Float32(1), Float32(5)), Untransformed())
    u2 = parameter(:σ_pist, 2.5230, (1., 5.), (1., 5.), Untransformed())

    @test u1.value == u2.value
    @test u1.valuebounds == u2.valuebounds
    @test u1.transform_parameterization == u2.transform_parameterization
    @test typeof(u1.value) == typeof(u2.value)
    @test eltype(u1.valuebounds) == eltype(u2.valuebounds)
    @test eltype(u1.transform_parameterization) == eltype(u2.transform_parameterization)
end

# probability
N = 10^2
u = parameter(:bloop, 2.5230, (1e-8, 5.), (1e-8, 5.), ModelConstructors.SquareRoot(); fixed = true)
v = parameter(:cat, 2.5230, (1e-8, 5.), (1e-8, 5.), ModelConstructors.Exponential(), Gamma(2.00, 0.1))

pvec =  ParameterVector{Float64}(undef, N)
for i in 1:length(pvec)
	global pvec[i] = (i%2 == 0) ? u : v
end
@testset "Check logpdf/pdf function approximations" begin
    @test logpdf(pvec) ≈ 50*logpdf(v)
    @test pdf(pvec) ≈ exp(50*logpdf(v))
end

updated = ModelConstructors.update(pvec, ones(length(pvec)))
ModelConstructors.update!(pvec, ones(length(pvec)))

@testset "Check if update! preserves dimensions and values" begin
    @test all(updated .== pvec)
    @test logpdf(pvec) == logpdf(updated)
end

# test we only update non-fixed parameters
@testset "Ensure only non-fixed parameters are updated" begin
    for p in pvec
        if p.fixed
            @test p.value == 2.5230
        elseif isa(p, Parameter)
            @test p.value == one(Float64)
        end
    end
end

# vector of new values must be the same length
@testset "Ensure update! enforces the same length of the parameter vector being updated" begin
    @test_throws BoundsError ModelConstructors.update!(pvec, ones(length(pvec)-1))
end

@testset "Ensure parameters being updated are of the same type." begin
    for w in [parameter_ad(:moop, 3.0, fixed=false), parameter_ad(:moop, 3.0; scaling = log, fixed=false)]
        # new values must be of the same type
        @test_throws ErrorException parameter_ad(w, one(Int))

        # new value is out of bounds
        @test_throws ParamBoundsError parameter_ad(w, -1.)
    end
end

@testset "Ensure parameter value types can be changed when forced by keyword." begin
    for w in [parameter_ad(:moop, 1.0, fixed=false), parameter_ad(:moop, 1.0; scaling = log, fixed=false)]
        # new values must be of the same type
        @test typeof(parameter_ad(w, one(Int); change_value_type = true).value) == Int
    end
end

# Tests moved from DSGE.jl core.jl
# UnscaledParameter, fixed=false
α =  parameter(:α, 0.1596, (1e-5, 0.999), (1e-5, 0.999), ModelConstructors.SquareRoot(), Normal(0.30, 0.05), fixed=false)
@testset "Test non-fixed UnscaledParameter" begin
    @test isa(α, UnscaledParameter)
    @test α.key == :α
    @test isa(α.prior.value, Normal)
    @test α.prior.value.μ == 0.3
    @test α.description == "No description available."
    @test α.tex_label == ""
    @test isa(α.transform, ModelConstructors.SquareRoot)
    @test get_untransformed_values(α) == α.value
end

# UnscaledParameter, fixed = true
α_fixed =  parameter(:α_fixed, 0.1596, (1e-5, 0.999), (1e-5, 0.999), ModelConstructors.Untransformed(), Normal(0.30, 0.05), fixed=true)
@testset "Test fixed UnscaledParameter" begin
    @test α_fixed.transform_parameterization == (0.1596,0.1596)
    @test isa(α_fixed.transform, ModelConstructors.Untransformed)
    @test get_untransformed_values(α_fixed) == α_fixed.value
end

# UnscaledParameter, fixed = true, transform should be overwritten given fixed
α_fixed =  parameter(:α_fixed, 0.1596, (1e-5, 0.999), (1e-5, 0.999), ModelConstructors.SquareRoot(), Normal(0.30, 0.05), fixed=true)
@testset "Test fixed UnscaledParameter, ensuring transform is overwritten" begin
    @test isa(α_fixed.transform, ModelConstructors.Untransformed)
end



# Fixed UnscaledParameter, minimal constructor
δ = parameter(:δ, 0.025)
@testset "Test fixed UnscaledParameter minimal constructor" begin
    @test δ.fixed
    @test δ.transform_parameterization == (0.025, 0.025)
    @test δ.valuebounds == (0.025, 0.025)
end

# Scaled parameter
β = parameter(:β, 0.1402, (1e-5, 10.), (1e-5, 10.), ModelConstructors.Exponential(), GammaAlt(0.25, 0.1), fixed=false,  scaling = x -> (1 + x/100)\1, description="β: Discount rate.", tex_label="\\beta ")
@testset "Test ScaledParameter constructor" begin
    @test isa(β, ScaledParameter)
    @test isa(β.prior.value, Gamma)
    @test isa(β.transform, ModelConstructors.Exponential)
    @test get_untransformed_values(β) == β.scaledvalue
end

# Invalid transform
@testset "Ensure error thrown on invalid transform" begin
    @test_throws UndefVarError α_bad = parameter(:α, 0.1596, (1e-5, 0.999), (1e-5, 0.999),
                                                 InvalidTransform(), Normal(0.30, 0.05), fixed=false)
end


# Arithmetic with parameters
@testset "Check arithmetic with parameters" begin
    @test promote_type(AbstractParameter{Float64}, Float16) == Float64
    @test promote_type(AbstractParameter{Float64}, Int8) == Float64
    ## @test promote_rule(AbstractParameter{Float64}, Float16) == Float64
    ## @test promote_rule(AbstractParameter{Float64}, Int8) == Float64
    @test δ + δ == 0.05
    @test δ^2 == 0.025^2
    @test -δ == -0.025
    @test log(δ) == log(0.025)
end

# transform_to_real_line and transform_to_model_space
cx = 2 * (α - 1/2)
@testset "Check parameter transformations for optimization" begin
    @test abs(transform_to_real_line(α) - cx / sqrt(1 - cx^2)) < .001
    @test transform_to_real_line(δ) == 0.025
end

# Check parameters can be declared as vectors
@testset "Test vector-valued parameters" begin
    p = parameter(:test, ones(5), (0.0, 5.), (0., 5.0), Untransformed(), MvNormal(ones(5), ones(5)), fixed = false, scaling = x -> x/2)
    @test all(ones(5) ./ 2 .== parameter(p, ones(5)).scaledvalue)
end

# Check we can update only specific parameter values in a ParameterVector
@testset "Test updating specific values of a ParameterVector" begin
    # Check you can overwrite values which are unfixed
    global pvec = ParameterVector{Float64}(undef, 3)
    pvec[1] = parameter(:a, 1., (0., 3.), (0., 3.), fixed = false)
    pvec[2] = parameter(:b, 1.)
    pvec[3] = parameter(:c, 1., (0., 3.), (0., 3.), fixed = false)
    global vals = [2., 2.]
    update!(pvec, vals, BitArray([true, false, true]))
    @test map(x -> x.value, pvec) == [2., 1., 2.]

    # Check that overwriting fixed parameter values does not work, even if you pass
    # the BitArray telling update! to change the values.
    pvec[1] = parameter(:a, 1.)
    pvec[2] = parameter(:b, 1.)
    pvec[3] = parameter(:c, 1.)
    global vals = [2., 2.]
    update!(pvec, vals, BitArray([true, false, true]))
    @test map(x -> x.value, pvec) == [1., 1., 1.]
end

@testset "Broadcasting with Parameter types" begin
    a = parameter(:a, 1., (0., 1.), (0., 1.), Untransformed(), fixed = false)
    b = parameter(:b, 1., (0., 1.), (0., 1.), Untransformed(), fixed = false, scaling = x -> x / 100.)

    rand_vals = rand(3)
    p=SteadyStateParameterGrid(:sspg, rand(3))
    p2=SteadyStateParameterGrid(:sspg, rand(3,2))
    rand1 = rand()

    @test a .* ones(3) == a.value * ones(3)
    @test a .* rand_vals == a.value * rand_vals
    @test b .* ones(3) == b.scaledvalue * ones(3)
    @test b .* rand_vals == b.scaledvalue * rand_vals
    @test p .* 1 == p.value
    @test p .* rand1 == p.value .* rand1
    @test p .* rand_vals == p.value .* rand_vals
end

nothing
