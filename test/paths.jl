m = GenericModel()

# model paths. all this should work without errors
m.testing = true
addl_strings = ["foo=bar", "hat=head", "city=newyork"]
@testset "Check proper model paths" begin
    for fn in [:rawpath, :workpath, :tablespath, :figurespath]
        @eval $(fn)(m, "test")
        @eval $(fn)(m, "test", "temp")
        @eval $(fn)(m, "test", "temp", addl_strings)
    end
end
