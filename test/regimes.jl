using Test, ModelConstructors

u = parameter(:bloop, 2.5230, (1e-8, 5.), (1e-8, 5.), ModelConstructors.SquareRoot(); fixed = true)
ModelConstructors.set_regime_val!(u, 1, 2.5230)

# CURRENTLY ONLY TESTS VALUE SWITCHING, NO REGIME SWITCHING IN OTHER CASES
@testset "Regime switching with parameters" begin
    @test_throws ParamBoundsError ModelConstructors.set_regime_val!(u, 2, 0.)
    ModelConstructors.set_regime_val!(u, 2, 0.; override_bounds = true)

    @test !isempty(u.regimes)
    @test u.regimes[:value][1] == 2.5230
    @test u.regimes[:value][2] == 0.
    @test u.value == 2.5230
    @test ModelConstructors.regime_val(u, 1) == 2.5230
    @test ModelConstructors.regime_val(u, 2) == 0.
    @test_throws KeyError ModelConstructors.regime_val(u, 3)
    ModelConstructors.toggle_regime!(u, 2)
    @test u.value == 0.
    @test ModelConstructors.ModelConstructors.regime_val(u, 1) == 2.5230
    @test ModelConstructors.ModelConstructors.regime_val(u, 2) == 0.
    ModelConstructors.toggle_regime!(u, 1)
    @test u.value == 2.5230
    @test ModelConstructors.regime_val(u, 1) == 2.5230
    @test ModelConstructors.regime_val(u, 2) == 0.
end

    nothing
