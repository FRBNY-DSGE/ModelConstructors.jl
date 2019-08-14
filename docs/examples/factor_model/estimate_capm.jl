# using ModelConstructors
include("../../../src/ModelConstructors.jl")
using FredData, DSGE, Dates, DataFrames, OrderedCollections
include("capm.jl")

### Estimate Sharpe's single factor model
# R_{it} = α_i + β_i R_{Mt} + ϵ_{it}, i = 1,...,N; t = 1,...,T
# where R_{Mt} is the excess return on a market index in time period t,
# and ϵ_{it} is an i.i.d. normally distributed mean zero shock with variance σ_i^2

### Estimate with SMC
N = 3 # number of asset returns

## Construct data matrix and constant data
fredseries = Array{FredSeries, 1}(undef, 5)
f = Fred()
start_date = Date("2009-12-31", "yyyy-mm-dd")
end_date = Date("2018-12-31", "yyyy-mm-dd")
# We use SP500, NASDAQ, russell 300 growth, wilshire micro-cap
for (i,s) in enumerate(["SP500", "NASDAQCOM", "RU3000GTR", "WILLMICROCAPPR", "DTB3"])
    fredseries[i] = get_data(f, string(s); frequency = "q",
                             observation_start = string(start_date),
                             observation_end   = string(end_date))
end

# Extract dataframe from each series and merge on date
data = DataFrame(date = DSGE.get_quarter_ends(start_date, end_date))
for i in 1:length(fredseries)
    if isassigned(fredseries, i)
        series = fredseries[i]
        series_id = Symbol(series.id)
        rename!(series.df, :value => series_id)
        map!(x->DSGE.lastdayofquarter(x), series.df[:date], series.df[:date])
        data = join(data, series.df[[:date, series_id]], on = :date, kind = :outer)
    end
end

# Change the dates to be the last day of each quarter
n_rows, n_cols = size(data)
for i = 1:n_rows
    data[i,:date] = Dates.lastdayofquarter(data[i,:date])
end

logdata = deepcopy(data)
logdata[:SP500] = log.(logdata[:SP500])
logdata[:NASDAQCOM] = log.(logdata[:NASDAQCOM])
logdata[:RU3000GTR] = log.(logdata[:RU3000GTR])
logdata[:WILLMICROCAPPR] = log.(logdata[:WILLMICROCAPPR])
lik_data = diff(Matrix{Float64}(Matrix{Float64}(logdata[[:NASDAQCOM, :RU3000GTR, :WILLMICROCAPPR]])'), dims = 2)
market_data = Matrix{Float64}((diff(logdata[:SP500]) - logdata[:DTB3][2:end] ./ 100)')

## Construct likelihood function:
# likelihood function is just R_{it} ∼ N(α_i + β_i R_{Mt}, σ_i)
# parameters to estimate are α_i, β_i, σ_i
# data is a time series of individual factor returns
# use S&P 500 data and a returns on a couple stocks. To get into returns,
# just take the log of the prices and regress those on each other.
function likelihood_fnct(p, d)
    # we assume the ordering of (α_i, β_i, σ_i)
    Σ = Matrix{Float64}(I,N,N)
    α = Vector{Float64}(N)
    β = Vector{Float64}(N)
    for i in 1:N
        α[i]   = p[i * 3 - 2]
        β[i]   = p[i * 3 - 2]
        Σ[i,i] = p[i * 3]^2
    end
    det_Σ = det(Σ)
    inv_Σ = inv(Σ)
    term1 = -N / 2 * log(2 * π) - 1 /2 * log(det_Σ)
    logprob = 0.
    errors = d .- α .- β .* market_data
    for t in 1:size(d,2)
        logprob += term1 - 1/2 * dot(errors, inv_Σ * errors)
    end
    return exp(logprob)
end

m = CAPM()
