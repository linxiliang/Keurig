# This file defines the appropriate modules and functions

# Module file defining the market class
struct BasicMarket
    Z::Array{Float64, 1}
    X::Array{Float64, 2}
    G::Array{Float64, 1}
    N::Int64
    M::Int64
    A::Int64
    sales::Array{Float64, 1}
end

struct Market
    market::BasicMarket
    hshare::Float64
    cshare::Array{Float64, 1}
    Market(market::BasicMarket) = new(market, A/M,
    sales./(consumption_rate*(N+M.*G)))
end

struct FullMarket
	sig::Float64
    market::Market
	meanU::Array{Float64, 1}
    FullMarket(sig::Float64, market::Market) = new(sig, market,
	xifun(sig, market))
end

# Nested Fixed Point Algorithm
function xifun(sig::Float64, m::Market)
    const scaling_param = sqrt(1./pi)
    const nprod = length(m.market.sales)
    meanU = zeros(Float64, nprod)
    err = 1.0
    while (err >= 1e-16)
        UMat = exp.(meanU .+ sqrt(2.) * sig * hermite_nodes')
        PMat_N = UMat ./ (sum(UMat, 1) .+ 1)
        PMat_M = UMat .* m.market.G ./ (sum(UMat .* m.market.G, 1).+1)
        PMat = m.market.N./(m.market.M * m.market.G .+ m.market.N) .* PMat_N +
        m.market.M * m.market.G/(m.market.M+m.market.N) .* PMat_M
        SVec = scaling_param * sum(PMat .* hermite_wts', 2)
        # Nested Fix Point Algorithmß
        meanU_new = meanU + log.(m.cshare) - log.(SVec)
        err = sum((meanU_new - meanU).^2)
        #println("The error is $(err)")
        meanU = meanU_new
    end
    return meanU[:,1]
end

# Inferior to the above Gauss-Hermite Method.
function rxifun(sig::Float64, m::Market)
    const nprod = length(m.market.sales)
    meanU = zeros(Float64, nprod)
    err = 1.0
    # Random Draws of tastes
    rnodes = randn(300000)
    while (err >= 1e-16)
        UMat = exp.(meanU .+ sig * rnodes')
        PMat_N = UMat ./ (sum(UMat, 1).+1)
        PMat_M = UMat .* m.market.G ./ (sum(UMat .* m.market.G, 1).+1)
        PMat = m.market.N./(m.market.M * m.market.G .+ m.market.N) .* PMat_N +
        m.market.M * m.market.G/(m.market.M+m.market.N) .* PMat_M
        SVec = mean(PMat, 2)
        # Nested Fix Point Algorithm
        meanU_new = meanU + log.(m.cshare) - log.(SVec)
        err = sum((meanU_new - meanU).^2)
        #println("The error is $(err)")
        meanU = meanU_new
    end
    return meanU[:,1]
end
