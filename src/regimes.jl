function set_regime_val!(p::Parameter{S},
                         i::Int64, v::S) where S <: Float64
    if !haskey(p.regimes, :value)
        p.regimes[:value] = Dict{Int64,S}()
    end
    p.regimes[:value][i] = v
    return v
end

function regime_val(p::Parameter{S}, i::Int64) where S <: Float64
    if !haskey(p.regimes, :value) || !haskey(p.regimes[:value], i)
        @error "get_regime_val(), Input Error: No regime $(i)"
    end
    return p.regimes[:value][i]
end

function toggle_regime!(p::Parameter{S}, i::Int64) where S <: Float64
    for field in [:value, :valuebounds, :transform_parameterization,
                  :transform, :prior, :fixed]
        if haskey(p.regimes, field) && haskey(p.regimes[field], i)
            if field == :value
                p.value = p.regimes[field][i]
            elseif field == :valuebounds
                p.valuebounds = p.regimes[field][i]
            elseif field == :transform_parameterization
                p.transform_parameterization = p.regimes[field][i]
            elseif field == :transform
                p.transform = p.regimes[:transform][i]
            elseif field == :prior
                p.prior = p.regimes[:transform][i]
            elseif field == :fixed
                p.transform = p.regimes[:transform][i]
            end
        else
            #@error "get_regime_val(), Input Error: No regime $(i)"
        end
    end
end
