# This file simulates a list of markets given consumer preferences
srand(1232323)
struct SimulationParameters
    pref::Array{Float64, 1}
    sigma::Float64
    market_size::Array{Int64, 1}
    num_keurig::Int64
    num_ground::Int64
    num_market::Int64
    T::Int64
    SimulationParameters(pref::Array{Float64, 1}, sigma::Float64,
    market_size::Array{Int64, 1}, num_keurig::Int64, num_ground::Int64,
    T::Int64) = new(pref, sigma, market_size, num_keurig, num_ground,
    length(market_size), T)
end

num_ground = 5
num_keurig = 3
keurig = [zeros(num_ground); ones(num_keurig)]
pref = 4*(rand(num_ground + num_keurig) - 0.5)
kpref = 2.0 * keurig
pref = pref + keurig
push!(pref, -2.0)
sigma = 2.0;

num_market = 30
market_size = [rand(10000:50000) for i in 1:num_market]
T = 100;
SP = SimulationParameters(pref, sigma, market_size, num_keurig, num_ground, T)

# Simulate market outcomes
function simulateMKT(SP::SimulationParameters)
    # Simulate a price vector
    MKT_list = []
    G = [ones(Float64, SP.num_ground)]
    for m in 1:SP.num_market
        M = SP.market_size[m]
        N = zeros(Int64, SP.T)
        A = zeros(Int64, SP.T)
        sales = zeros(Float64, SP.num_ground + SP.num_keurig, SP.T)
        # Simulate household level consumer preferences
        i_pref = SP.sigma * randn(M)
        prices = 0.5 + randn((SP.num_ground + SP.num_keurig)*T)*.20
        meanU  = repeat(pref[1:(end-1)], outer=T) .+ pref[end]*prices
        meanU  = transpose(reshape(meanU, (SP.num_ground + SP.num_keurig), T))
        xi     = randn((SP.num_ground + SP.num_keurig)*T)
        xi     = transpose(reshape(xi, (SP.num_ground + SP.num_keurig), T))
        meanU  = meanU + xi
        for i in 1:M
            adoption = 0
            eps1 = -log.(-log.(rand((SP.num_ground + SP.num_keurig)*T)))
            eps1 = transpose(reshape(eps1, (SP.num_ground + SP.num_keurig), T))
            u_i_1 = meanU + i_pref[i] + eps1
            u_i_0 =  -log.(-log.(rand((SP.num_ground + SP.num_keurig)*T)))
            for t in 1:SP.T
                # Inside here, I need to simulate both purchase of coffee and consumer adoption
                # Fix this part in the future
                if rand()>=0.8 && adoption==0
                    adoption = 1
                    A[t] += 1
                    N[t:T] += 1
                end

                # Given Adoption status, get sales
                if adoption==1
                    umax, ind = findmax([u_i_0[t]; u_i_1[t, :]])
                else
                    umax, ind = findmax([u_i_0[t]; u_i_1[t, 1:SP.num_ground]])
                end

                # Store sales
                if ind!=1
                    sales[ind-1, t] += 1
                end

                # Push into the list
                if (i == M)
                    Z = [1.0, 145.6];
                    G = 1 - keurig
                    nprod = SP.num_ground + SP.num_keurig
                    # p_ind_mat = eye(SP.num_ground + SP.num_keurig)[:, 1:(end-1)]
                    p_ind_mat = eye(SP.num_ground + SP.num_keurig)
                    # X = hcat(p_ind_mat, keurig, prices[(t-1)*nprod+1:(t*nprod)])
                    X = hcat(p_ind_mat, prices[(t-1)*nprod+1:(t*nprod)])
                    non_zero_ind = find(sales[:, t])
                    G = G[non_zero_ind]
                    X = X[non_zero_ind, :]
                    sales_t = sales[non_zero_ind, t]
                    mkt = BasicMarket(Z, X, G, N[t], M, A[t], sales_t)
                    mkt = Market(mkt)
                    push!(MKT_list, mkt)
                end
            end
        end
    end
    return MKT_list
end

market_list = simulateMKT(SP)
