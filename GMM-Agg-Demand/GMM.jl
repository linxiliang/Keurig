# Load necessary library
using FastGaussQuadrature
global (hermite_nodes, hermite_wts) = gausshermite(20);
const consumption_rate = 3;

# Module file defining the market class
struct BasicMarket
    Z::Array{Float64, 1}
    X::Array{Float64, 2}
    K::Array{Float64, 1}
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
    sales./(consumption_rate*(N+M.*K)))
end

Z = [1.0, 145.6];
X = [1.0 0.0 0.0 0.0 0.16; 0.0 1.0 0.0 1.0 0.48; 0.0 0.0 1.0 1.0 0.50];
K = [1.0, 0.0, 0.0];
N = 100;
M = 200;
A = 10;
sales = [50.0, 10.0, 15.0];
bmkt = BasicMarket(Z, X, K, N, M, A, sales);
mkt = Market(bmkt);

# Nested Fixed Point Algorithm
function xifun(sig::Float64, m::Market)
    const scaling_param = sqrt(1./pi)
    const nprod = length(m.market.sales)
    meanU = zeros(Float64, nprod)
    err = 1
    while (err >= 1e-14)
        UMat = exp.(meanU .+ sqrt(2.) * sig * hermite_nodes')
        PMat_N = UMat ./ (sum(UMat, 1).+1)
        PMat_M = UMat .* m.market.K ./ (sum(UMat .* m.market.K, 1).+1)
        PMat = m.market.N./(m.market.M * m.market.K .+ m.market.N) .* PMat_N +
        m.market.M * m.market.K/(m.market.M+m.market.N) .* PMat_M
        SVec = scaling_param * sum(PMat .* hermite_wts', 2)
        # Nested Fix Point Algorithm
        meanU_new = meanU + log.(m.cshare) - log.(SVec)
        err = sum((meanU_new - meanU).^2)
        #println("The error is $(err)")
        meanU = meanU_new
    end
    return meanU
end


function rxifun(sig::Float64, m::Market)
    const nprod = length(m.market.sales)
    meanU = zeros(Float64, nprod)
    err = 1
    # Random Draws of tastes
    rnodes = randn(300000)
    while (err >= 1e-14)
        UMat = exp.(meanU .+ sig * rnodes')
        PMat_N = UMat ./ (sum(UMat, 1).+1)
        PMat_M = UMat .* m.market.K ./ (sum(UMat .* m.market.K, 1).+1)
        PMat = m.market.N./(m.market.M * m.market.K .+ m.market.N) .* PMat_N +
        m.market.M * m.market.K/(m.market.M+m.market.N) .* PMat_M
        SVec = mean(PMat, 2)
        # Nested Fix Point Algorithm
        meanU_new = meanU + log.(m.cshare) - log.(SVec)
        err = sum((meanU_new - meanU).^2)
        #println("The error is $(err)")
        meanU = meanU_new
    end
    return meanU
end

# Check whether xifun and rxifun give the same results
err_vec = [sum((xifun(Float64(i), mkt) - rxifun(Float64(i), mkt)).^2) for i in 1:20]

# Outer Constructor to construct types
FullMarket(market::BasicMarket, pref::Pref) = FullMarket(BasicMarket)
function Derived(Z, X, )
market(Z::Array{Float64, 2}, X::Array{Float64, 2}, N::Int64, M::Int64, A::Int64,
sales::Array{Float64, 1})


# Gauss Hermite Nodes
sum((sqrt(2.)*hermite_nodes).^2 .* hermite_wts)*(sqrt(1./pi))^1
exp(sqrt(2.)*σ*hermitenodes' .+ μ)
exp(sqrt(2.)*σ*hermitenodes' .+ μ)
