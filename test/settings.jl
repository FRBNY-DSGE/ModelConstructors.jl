m = AnSchorfheide()

# settings
# settings - boolean, string, and number. adding to model. overwriting. filestrings. testing/not testing.
n_mh_blocks = Setting(:n_mh_blocks, 22) # short constructor
reoptimize = Setting(:reoptimize, false)
vint = Setting(:data_vintage, "REF", true, "vint", "Date of data") # full constructor

@testset "Check settings corresponding to parameters" begin
    @test promote_rule(Setting{Float64}, Float16) == Float64
    @test promote_rule(Setting{Bool}, Bool) == Bool
    @test promote_rule(Setting{String}, String) == String
    @test convert(Int64, n_mh_blocks) == 22
    @test convert(String, vint) == "REF"

    @test get_setting(m, :n_mh_blocks) == m.settings[:n_mh_blocks].value
    m.testing = true
    @test get_setting(m, :n_mh_blocks) == m.test_settings[:n_mh_blocks].value
    @test ModelConstructors.filestring(m) == "_test"

    m.testing = false
    m <= Setting(:n_mh_blocks, 5, true, "mhbk", "Number of blocks for Metropolis-Hastings")
    @test m.settings[:n_mh_blocks].value == 5
    @test occursin(r"^\s*_mhbk=5_vint=(\d{6})", ModelConstructors.filestring(m))
    ModelConstructors.filestring(m, "key=val")
    ModelConstructors.filestring(m, ["key=val", "foo=bar"])
    m.testing = true

  # Overwriting settings
    a = gensym() # unlikely to clash
    b = gensym()
    m <= Setting(a, 0, true, "abcd", "a")
    m <= Setting(a, 1)
    @test m.test_settings[a].value == 1
    @test m.test_settings[a].print == true
    @test m.test_settings[a].code == "abcd"
    @test m.test_settings[a].description == "a"
    m <= Setting(b, 2, false, "", "b")
    m <= Setting(b, 3, true, "abcd", "b1")
    @test m.test_settings[b].value == 3
    @test m.test_settings[b].print == true
    @test m.test_settings[b].code == "abcd"
    @test m.test_settings[b].description == "b1"
end
