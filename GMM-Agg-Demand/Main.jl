# GMM Estimation of Aggregate Model

# Xiliang Lin

# December 2017

#= Pseudo Codes
Compute market share
Given assumptions on k servings per day
N be the number of Keurig adopters, and M be the number of non-adopter
M+N is the total population of coffee consumers

Hardware Market
The share of adopters N/(M+N), and potential market size is M
Adoption share is then s_purchased = N_Purchased/M

Software Market
the market shares for ground coffee brands
s_j = sales/(N*k + M*k)
the market shares for Keurig coffee brands
s_j = sales/(N*k)

1. Given θ, compute ...
2. Given utilities, compute shares
3. Share inversion to get ξs
4. Given ξ, get the GMM objective
5. Optimization routine to optimize GMM objective
=#

#= Codes
Setting

class(number of markets, population, sales,
number of Keurig adopters, number non-adopters)

μ_j = brand_intercept_j + flavor_fixed_effects_j + β * logP_j (or P_j) + ξ_jt

What random effects shall I use?
μ_ij = γ_i + μ_j
or, I will allow β to be random.

Type market
market(Z::Array{Float64, 2}, X::Array{Float64, 2}, N::Int64, M::Int64, A::Int64,
sales::Array{Float64, 1})
# Functions of the market
hardware.market.share = A/M
software.market.share = sales/(N*k + M*k) if ground
                      = sales/(N*k) if Keurig
# Give preferences compute the simulated market share


=#
