using ModelConstructors, Test

@testset "Prior computation when parameters are transformed to the real line" begin
    p = Vector{AbstractParameter{Float64}}(undef, 0)
    p1 = Vector{AbstractParameter{Float64}}(undef, 0)
    p2 = Vector{AbstractParameter{Float64}}(undef, 0)
    push!(p, parameter(:a, 12.7314, (-15., 15.), (-15., 15.), Untransformed(), Normal(4., 1.5), fixed = false))
    push!(p, parameter(:b, 1.1066, (1., 10.), (1e-5, 0.), Exponential(), Normal(1.25, 0.12), fixed = false))
    push!(p, parameter(:c, 0.5347, (1e-5, 1. - 1e-5), (1e-5, 1. - 1e-5), SquareRoot(), BetaAlt(0.7, 0.1), fixed = false))
    push!(p, parameter(:d, 0.1402, (1e-5, 10.), (1e-5, 0.), Exponential(), GammaAlt(0.25, 0.1), fixed = false))
    push!(p, parameter(:e, 2.5230, (1e-5, 5.), (1e-5, 5.), Exponential(), RootInverseGamma(2, 0.1), fixed = false))

    push!(p1, parameter(:a, 12.7314, (-15., 15.), (-15., 15.), Untransformed(), Normal(4., 1.5), fixed = false))
    push!(p1, parameter(:b, 1.1066, (1., 10.), (1e-5, 0.), Untransformed(), Normal(1.25, 0.12), fixed = false))
    push!(p1, parameter(:c, 0.5347, (1e-5, 1. - 1e-5), (1e-5, 1. - 1e-5), Untransformed(), BetaAlt(0.7, 0.1), fixed = false))
    push!(p1, parameter(:d, 0.1402, (1e-5, 10.), (1e-5, 0.), Untransformed(), GammaAlt(0.25, 0.1), fixed = false))
    push!(p1, parameter(:e, 2.5230, (1e-5, 5.), (1e-5, 5.), Untransformed(), RootInverseGamma(2, 0.1), fixed = false))

    push!(p2, parameter(:a, 1.1066, (1., 10.), (0., 0.), Exponential(), Normal(1.25, 0.12), fixed = false))
    push!(p2, parameter(:b, 1.1, (1., 10.), (0., 0.), Exponential(), Normal(1.25, 0.12), fixed = false))

    x_in_R = ModelConstructors.transform_to_real_line(p)
    logpdfs = []
    check_correct = []
    for i = 1:length(p)
        if ModelConstructors.hasprior(p[i])
            push!(logpdfs, logpdf(get(p[i].prior), transform_to_model_space(p[i], x_in_R[i])) +
                  log(abs(differentiate_transform_to_model_space(p[i], x_in_R[i]))))
            push!(check_correct, logpdfs[i] - log(abs(differentiate_transform_to_model_space(p[i], x_in_R[i]))))
        end
    end

    analytical1_p2(x) = map(y -> logpdf(get(y.prior), transform_to_model_space(y, ModelConstructors.transform_to_real_line(y))) +
                            log(abs(exp(ModelConstructors.transform_to_real_line(y)))), x)
    analytical2_p2(x) = map(y -> exp.(logpdf(get(y.prior), transform_to_model_space(y, ModelConstructors.transform_to_real_line(y)))) .* (abs(exp(ModelConstructors.transform_to_real_line(y)))), x)
    @test prior(p) == sum(check_correct)
    @test prior(p) != prior(p, x_in_R)
    @test prior(p) == prior(p1, map(x -> x.value, p1))
    @test prior(p2, ModelConstructors.transform_to_real_line(p2)) == sum(analytical1_p2(p2))
    @test exp(prior(p2, ModelConstructors.transform_to_real_line(p2))) == prod(analytical2_p2(p2))
end
