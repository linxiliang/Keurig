# This file simulates a list of markets given consumer preferences
srand(94554654)
struct SimulationParameters
    pref::Array{Float64, 1}
    sigma::Float64
    market_size::Array{Int64, 1}
    num_keurig::Int64
    num_ground::Int64
    T::Int64
    num_market::Int64
    SimulationParameters(pref::Array{Float64, 1}, sigma::Float64,
    market_size::Array{Int64, 1}, num_keurig::Int64, num_ground::Int64,
    T::Int64) = new(pref, sigma, market_size, num_keurig, num_ground,
    T, length(market_size))
end

num_ground = 5
num_keurig = 3
keurig = [zeros(num_ground); ones(num_keurig)]
pref = rand(num_ground + num_keurig)*0.25 - 3.0
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
    G = [ones(Float64, SP.num_ground); zeros(Float64, SP.num_keurig)]
    for m in 1:SP.num_market
        M = SP.market_size[m]
        NProd = SP.num_ground + SP.num_keurig
        N = zeros(Int64, SP.T)
        A = zeros(Int64, SP.T)
        sales = zeros(Float64, SP.T, NProd)
        # Simulate household level consumer preferences
        i_pref = SP.sigma * randn(M)
        prices = 0.5 + randn(SP.T, NProd)*.20
        meanU  = pref[1:(end-1)]' .+ pref[end]*prices
        xi     = randn(SP.T, NProd)
        meanU  = meanU + xi
        adoption = zeros(Int64, M)
        for t in 1:SP.T
            # Try to draw from multinomial distribution
            avail = rand(NProd).<=0.9
            if all(avail .== false)
                avall[1] = true
            end
            gindicator = (avail .& (G .== 1.0))
            unshift!(avail, true)
            unshift!(gindicator, true)
            avail_vec = find(avail)
            gavail_vec = find(gindicator)
            for i in 1:M
                expU = [1.0 ; exp.(meanU[t,:] + i_pref[i])]
                p_full = expU[avail]/sum(expU[avail])
                p_ground = expU[gindicator]/sum(expU[gindicator])

                # Inside here, I need to simulate both purchase of coffee and consumer adoption
                if rand()>=0.0001 && adoption[i]==0
                    adoption[i] = 1
                    A[t] += 1
                    N[t:T] += 1
                end

                # Given Adoption status, get sales
                if adoption[i]==1
                    simv = rand(Multinomial(1, p_full))
                    _, indx = findmax(simv)
                    ind = avail_vec[indx]
                else
                    simv = rand(Multinomial(1, p_ground))
                    _, indx = findmax(simv)
                    ind = gavail_vec[indx]
                end

                # Store sales
                if ind!=1
                    sales[t, ind-1] += 1 * consumption_rate
                end
            end
            # Push into the list
            Z = [1.0, 145.6];
            p_ind_mat = eye(NProd)
            X = hcat(p_ind_mat, prices[t,:])
            non_zero_ind = find(sales[t, :])
            G_t = G[non_zero_ind]
            X_t = X[non_zero_ind, :]
            sales_t = sales[t, non_zero_ind]
            # In this testing
            mkt = BasicMarket(Z, X_t, G_t, N[t], M, A[t], sales_t)
            mkt = Market(mkt)
            push!(MKT_list, mkt)
        end
    end
    return MKT_list
end

market_list = simulateMKT(SP)
