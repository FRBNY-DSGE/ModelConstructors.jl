"""
```
default_settings!(m::AbstractModel)
```

Default Settings are constructed, initialized and added to `m.settings`.
"""
function default_settings!(m::AbstractModel)

    settings = m.settings

    # I/O File locations
    saveroot = normpath(joinpath(dirname(@__FILE__), "..","save"))
    datapath = normpath(joinpath(dirname(@__FILE__), "..","save","input_data"))

    settings[:saveroot] = Setting(:saveroot, saveroot, "Root of data directory structure")
    settings[:dataroot] = Setting(:dataroot, datapath, "Input data directory path")

    # Data settings for released and conditional data. Default behavior is to set vintage
    # of data to today's date.
    vint = Dates.format(now(), "yyyymmdd")
    settings[:data_vintage] = Setting(:data_vintage, vint, true, "vint",
        "Data vintage")
    settings[:data_id] = Setting(:data_id, 3,
        "Dataset identifier")

    return settings
end

"""
```
default_test_settings!(m::AbstractModel)
```

The following Settings are constructed, initialized and added to
`m.test_settings`. Their purposes are identical to those in
`m.settings`, but these values are used to test DSGE.jl.

### I/O Locations and identifiers
- `saveroot::Setting{String}`: A temporary directory in /tmp/
- `dataroot::Setting{String}`: dsgeroot/test/reference/
- `data_vintage::Setting{String}`: \"_REF\"

### Metropolis-Hastings
- `n_mh_simulations::Setting{Int}`: 100
- `n_mh_blocks::Setting{Int}`: 1
- `n_mh_burn::Setting{Int}`: 0
- `mh_thin::Setting{Int}`: 1
"""
function default_test_settings!(m::AbstractModel)

    test = m.test_settings

    # I/O
    dataroot = normpath(joinpath(dirname(@__FILE__), "..", "test", "reference", "input_data"))
    saveroot = mktempdir()

    # General
    test[:saveroot] = Setting(:saveroot, saveroot,
        "Where to write files when in test mode")
    test[:dataroot] = Setting(:dataroot, dataroot,
        "Location of input files when in test mode" )
    test[:data_vintage] = Setting(:data_vintage, "REF", true, "vint",
        "Reference data identifier")

    return test
end
