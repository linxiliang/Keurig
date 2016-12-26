#########################################################################
#
# Simple Discrete Choice Simulation 
# with Normal Consumer Heterogeneithy
# and State Dependence 
# Using Bayesian Estimation
# 
# Xiliang Lin
# Nov, 2015
#########################################################################
workspace() 
#Install missing packages
function PkgCheck(pkg::AbstractString)
    try
      eval(parse("using "*pkg))
    catch
      Pkg.add(pkg)
      eval(parse("using "*pkg))
    end
    return 0
end
#Load required packages
PkgCheck("Distributions")
PkgCheck("Mamba")

#------------------------------------------------------------------------#
#Simulate the data
srand(123)
#Set Preference Distributions
sigma = [0.9 0.01 0.0 0.01
         0.01 0.9 0.0 0.01
         0.0 0.0 0.09 0.0 
         0.01 0.01 0.0 0.04];
mu = [0, 0, -4, 0.5];
trueparam = MvNormal(mu, sigma);

#Simulate preference parameters
nhh = 10000
preferences=rand(trueparam, nhh)'
#No heterogeneity - to test the codes
#preferences = transpose(repmat(mu, 1, nhh))
preferences=convert(SharedArray, preferences)

#Let the time period be a binomial draw 
TBinom = Binomial(10, 0.5)
periods = rand(TBinom, nhh)
periods = convert(SharedArray, periods)

#Let the price of the three products be draw from uniform distribution
price = rand(sum(periods), 2)
price = hcat(rep(0, sum(periods)), price)
price = reshape(price', (3*sum(periods), 1))
price = convert(SharedArray, price)

#Simulate the latent utility draw
eps = rand(Gumbel(), sum(periods)*3, 1)
eps = convert(SharedArray, eps)
      
#Now simulate the purchasing sequences
#Using shared array to store the positions 
hh_pos = SharedArray(Int64, nhh, 2)
#Using shared array for purchase sequences
utils = SharedArray(Float64, 3*sum(periods))
purchases = SharedArray(Float64, 3*sum(periods))
state = SharedArray(Float64, 3*sum(periods))
@parallel for i=1:nhh
    if (i==1)
        hh_pos[i,1] = 1
    else 
        hh_pos[i,1] = 3*sum(periods[1:(i-1)])+1
    end 
    hh_pos[i, 2] = 3*sum(periods[1:i])
end

function util(j::Int64, prefer::Matrix, prices::Vector, state::Vector, noise::Vector)
    if (j == 1)
        return(noise[1])
    else
        #println("equation = $(prefer[j-1]) + $(prefer[3]) * $(prices[j]) + $(prefer[4]) * $(state[j]) + $(noise[j])\n")
        return(prefer[j-1] + prefer[3] * prices[j] + prefer[4] * state[j] + noise[j])
    end
end

@parallel for i=1:nhh
    for t = hh_pos[i, 1]:3:hh_pos[i, 2]
        utils[t:(t+2)] = [util(1, preferences[i,:]', price[t:(t+2)], state[t:(t+2)], eps[t:(t+2)]),
          util(2, preferences[i,:]', price[t:(t+2)], state[t:(t+2)], eps[t:(t+2)]),
         util(3, preferences[i,:]', price[t:(t+2)], state[t:(t+2)], eps[t:(t+2)])]
        purchases[t:(t+2)]= utils[t:(t+2)].==maximum(utils[t:(t+2)])
  if  (t != (hh_pos[i,2]-2))
         if (purchases[t]==true)
     state[(t+3):(t+5)] = state[t:(t+2)]
   else
     state[(t+3):(t+5)] = purchases[t:(t+2)]
   end
       end
    end
end

#------------------------------------------------------------------------#


## Data
hh = SharedArray(Int64, sum(periods)*3)
for i in 1:nhh 
   hh[hh_pos[i,1]:hh_pos[i,2]] = i
end

pdt  = Dict{Symbol, Any}(
  :hh=> hh,
  :y => purchases,
  :x => hcat(prod_intercepts, price, state)
)

## Model Specification
model = Model(
  y = Stochastic(1,
    (alpha, beta, rat, Xm, s2_c) ->
      begin
        mu = alpha[rat] + beta[rat] .* Xm
        MvNormal(mu, sqrt(s2_c))
      end,
    false
  ),

  alpha = Stochastic(1,
    (mu_alpha, s2_alpha) -> Normal(mu_alpha, sqrt(s2_alpha)),
    false
  ),

  alpha0 = Logical(
    (mu_alpha, xbar, mu_beta) -> mu_alpha - xbar * mu_beta
  ),

  mu_alpha = Stochastic(
    () -> Normal(0.0, 1000),
    false
  ),

  s2_alpha = Stochastic(
    () -> InverseGamma(0.001, 0.001),
    false
  ),

  beta = Stochastic(1,
    (mu_beta, s2_beta) -> Normal(mu_beta, sqrt(s2_beta)),
    false
  ),

  mu_beta = Stochastic(
    () -> Normal(0.0, 1000)
  ),

  s2_beta = Stochastic(
    () -> InverseGamma(0.001, 0.001),
    false
  ),

  s2_c = Stochastic(
    () -> InverseGamma(0.001, 0.001)
  )

)


## Initial Values
inits = [
  Dict(:y => rats[:y], :alpha => fill(250, 30), :beta => fill(6, 30),
       :mu_alpha => 150, :mu_beta => 10, :s2_c => 1, :s2_alpha => 1,
       :s2_beta => 1),
  Dict(:y => rats[:y], :alpha => fill(20, 30), :beta => fill(0.6, 30),
       :mu_alpha => 15, :mu_beta => 1, :s2_c => 10, :s2_alpha => 10,
       :s2_beta => 10)
]


## Sampling Scheme
scheme = [Slice([:s2_c], [10.0]),
          AMWG([:alpha], fill(100.0, 30)),
          Slice([:mu_alpha, :s2_alpha], [100.0, 10.0], :univar),
          AMWG([:beta], ones(30)),
          Slice([:mu_beta, :s2_beta], [1.0, 1.0], :univar)]
setsamplers!(model, scheme)


## MCMC Simulations
sim = mcmc(model, rats, inits, 10000, burnin=2500, thin=2, chains=2)
describe(sim)