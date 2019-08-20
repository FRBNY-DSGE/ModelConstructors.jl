using ModelConstructors
using Test, Distributions, InteractiveUtils, Nullables

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

# probability
N = 10^2
u = parameter(:bloop, 2.5230, (1e-8, 5.), (1e-8, 5.), ModelConstructors.SquareRoot(); fixed = true)
v = parameter(:cat, 2.5230, (1e-8, 5.), (1e-8, 5.), ModelConstructors.Exponential(), Gamma(2.00, 0.1))

pvec =  ParameterVector{Float64}(undef, N)
for i in 1:length(pvec)
	pvec[i] = (i%2 == 0) ? u : v
end
@testset "Check logpdf/pdf function approximations" begin
    @test logpdf(pvec) ≈ 50*logpdf(v)
    @test pdf(pvec) ≈ exp(50*logpdf(v))
end

updated = update(pvec, ones(length(pvec)))
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
    @test_throws AssertionError ModelConstructors.update!(pvec, ones(length(pvec)-1))
end

@testset "Ensure parameters being updated are of the same type." begin
    for w in [parameter(:moop, 3.0, fixed=false), parameter(:moop, 3.0; scaling = log, fixed=false)]
        # new values must be of the same type
        @test_throws MethodError parameter(w, one(Int))

        # new value is out of bounds
        @test_throws ParamBoundsError parameter(w, -1.)
    end
end

using DSGE
# subspecs
function sstest(m::AnSchorfheide)

    # Change all the fields of an unfixed parameter
    m <= parameter(:ι_w, 0.000, (0.0, .9999), (0.0,0.9999), ModelConstructors.Untransformed(), Normal(0.0,1.0), fixed=false,
                   description="ι_w: A new parameter.",
                   tex_label="\\iota_w")


    # Change an unfixed parameter to be fixed
    m <= parameter(:ι_p, 0.000, fixed=true,
                   description= "ι_p: Another new parameter",
                   tex_label="\\iota_p")


    # Change a fixed parameter
    m <= parameter(:δ, 0.02,  fixed=true,
                   description="δ: The capital depreciation rate.", tex_label="\\delta" )


    # Overwrite a fixed parameter with an unfixed parameter
    m <= parameter(:ϵ_p, 0.750, (1e-5, 10.),   (1e-5, 10.),     ModelConstructors.Exponential(),    GammaAlt(0.75, 0.4),        fixed=false,  scaling = x -> 1 + x/100,
                   description="ϵ_p: No description available.",
                   tex_label="\\varepsilon_{p}")

    steadystate!(m)
end

m = AnSchorfheide()
sstest(m)

@testset "Test steady-state parameters" begin
    @test m[:ι_w].value == 0.0
    @test m[:ι_w].valuebounds == (0.0, .9999)
    @test m[:ι_w].transform == ModelConstructors.Untransformed()
    @test m[:ι_w].transform_parameterization == (0.0,0.9999)
    @test isa(m[:ι_w].prior.value, Normal)

    @test m[:ι_p].value == 0.0
    @test m[:ι_p].valuebounds == (0.0, 0.0)
    @test isnull(m[:ι_p].prior)
    @test m[:ι_p].fixed == true

    @test m[:δ].value == 0.02
    @test m[:δ].valuebounds == (0.02, 0.02)
    @test isnull(m[:δ].prior)
    @test m[:δ].fixed == true

    @test m[:ϵ_p].value == 0.750
    @test m[:ϵ_p].transform == ModelConstructors.Exponential()
    @test isa(m[:ϵ_p].prior.value, Gamma)
    @test m[:ϵ_p].fixed==false
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
end

# UnscaledParameter, fixed = true
α_fixed =  parameter(:α_fixed, 0.1596, (1e-5, 0.999), (1e-5, 0.999), ModelConstructors.Untransformed(), Normal(0.30, 0.05), fixed=true)
@testset "Test fixed UnscaledParameter" begin
    @test α_fixed.transform_parameterization == (0.1596,0.1596)
    @test isa(α_fixed.transform, ModelConstructors.Untransformed)
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


m = AnSchorfheide()
let lastparam = parameter(:p, 0.0)
    for θ in m.parameters
        isa(θ, Parameter) && (lastparam = θ)
    end
    @testset "Check AnSchorfheide last parameter" begin
        @test isa(lastparam, Parameter)
        @test lastparam.value == 0.20*2.237937
    end
end
# transform_to_real_line and transform_to_model_space, acting on the entire parameter vector. they should be inverses!
pvec = m.parameters
vals = transform_to_real_line(pvec)
transform_to_model_space!(m, vals)
@testset "Check parameter transformations for optimization part 2" begin
    @test pvec == m.parameters
end

# all fixed parameters should be unchanged by both transform_to_real_line and transform_to_model_space
@testset "Check fixed parameters are unchanged by optimization transformations" begin
    for θ in m.parameters
        if θ.fixed
            @test θ.value == transform_to_real_line(θ)
            @test θ.value == transform_to_model_space(θ, θ.value)
        end
    end
end


# New Tests
@testset "Test vector-valued parameters" begin
    p = parameter(:test, ones(5), (0.0, 5.), (0., 5.0), Untransformed(), MvNormal(ones(5), ones(5)), fixed = false, scaling = x -> x/2)
    @test all(ones(5) ./ 2 .== parameter(p, ones(5)).scaledvalue)
end

nothing
