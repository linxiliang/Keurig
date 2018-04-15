# This file defines the appropriate modules and functions

# Module file defining the market class
struct BasicMarket
    Z::Array{Float64, 1} # A vector governing Adoption
    X::Array{Float64, 2} # Variables governing Purchases
    G::Array{Float64, 1} # A vector indicating whether the product is ground
    Price::Array{Float64, 1} # A vector product prices (per unit)
    N::Int64 # Number of cumulative Adopters
    M::Int64 # Number of consumers in the market
    A::Int64 # Adopters of the current period
    sales::Array{Float64, 1}
end

struct Market
    market::BasicMarket
    hshare::Float64
    cshare::Array{Float64, 1}
    Market(market::BasicMarket) = new(market, market.A/(market.M-market.N+market.A),
    market.sales ./ (consumption_rate*market.M))
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
    while (err >= 1e-9)
        UMat = exp.(meanU .+ m.market.Price .* (sqrt(2.) * sig * hermite_nodes'))
        PMat_N = UMat ./ (sum(UMat, 1) .+ 1)
        PMat_M = UMat .* m.market.G ./ (sum(UMat .* m.market.G, 1).+1)
        SVec_N = scaling_param * sum(PMat_N .* hermite_wts', 2)[:,1]
        SVec_M = scaling_param * sum(PMat_M .* hermite_wts', 2)[:,1]
        SVec = m.market.N/(m.market.M) .* SVec_N +
        (m.market.M - m.market.N)/(m.market.M) .* SVec_M
        # Nested Fix Point Algorithm
        meanU_new = meanU + log.(m.cshare) - log.(SVec)
        err = sum((meanU_new - meanU).^2)
        #println("The error is $(err)")
        meanU = meanU_new
    end
    # Compute gradient with respect to
    return meanU[:,1]
end

# Inferior to the above Gauss-Hermite Method.
function rxifun(sig::Float64, m::Market)
    const nprod = length(m.market.sales)
    meanU = zeros(Float64, nprod)
    err = 1.0
    rnodes = randn(3000)
    while (err >= 1e-9)
        UMat = exp.(meanU .+ m.market.Price .* (sig * rnodes'))
        PMat_N = UMat ./ (sum(UMat, 1).+1)
        PMat_M = UMat .* m.market.G ./ (sum(UMat .* m.market.G, 1).+1)
        SVec_N = mean(PMat_N, 2)
        SVec_M = mean(PMat_M, 2)
        SVec = m.market.N/(m.market.M) .* SVec_N +
        (m.market.M - m.market.N)/(m.market.M) .* SVec_M
        # Nested Fix Point Algorithm
        meanU_new = meanU + log.(m.cshare) - log.(SVec)
        err = sum((meanU_new - meanU).^2)
        #println("The error is $(err)")
        meanU = meanU_new
    end
    return meanU[:,1]
end

# Settings of a market
Z = [1.0, 145.6];
X = [1.0 0.0 0.0 0.0 0.16; 0.0 1.0 0.0 1.0 0.48; 0.0 0.0 1.0 1.0 0.50];
G = [1.0, 0.0, 0.0];
Price = X[:, end]
N = 200;
M = 200;
A = 10;
sales = [50.0, 10.0, 15.0];
bmkt = BasicMarket(Z, X, G, Price, N, M, A, sales);
mkt = Market(bmkt);
fmkt = FullMarket(2.0, mkt);

# Check whether xifun and rxifun give the same results
err_vec = [sum((xifun(Float64(i), mkt) - rxifun(Float64(i), mkt)).^2) for i in 1:20]


# Check whether xifun and rxifun give the same results
if (maximum(sqrt.(err_vec)./collect(1:20))<0.05)
    println("error checking passed! ")
else
    println("Two functions doesn't match!!!")
end
