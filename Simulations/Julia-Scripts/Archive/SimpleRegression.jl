using Distributions;
using Mamba;

## Settings
srand(12345);
tbeta = [1, 2];
n=1000;

## Generate Data
dt = Dict{Symbol, Any}(
  :x => hcat(ones(n,1),rand(n))
  )
dt[:y] = dt[:x] * tbeta + randn(n)

## Setup the model
model = Model(

  y = Stochastic(1,
    (mu, s2) ->  MvNormal(mu, sqrt(s2)),
    false
  ),

  mu = Logical(1,
    (x, beta) -> x * beta,
    false
  ),

  beta = Stochastic(1,
    () -> MvNormal([0,0], sqrt(1000))
  ),

  s2 = Stochastic(
    () -> InverseGamma(0.001, 0.001)
  )

)

## Sampling Scheme
## User-Defined Samplers

Gibbs_beta = Sampler([:beta],
  (beta, s2, x, y) ->
    begin
      beta_mean = mean(beta.distr)
      beta_invcov = invcov(beta.distr)
      Sigma = inv(x' * x / s2 + beta_invcov)
      mu = Sigma * (x' * y / s2 + beta_invcov * beta_mean)
      rand(MvNormal(mu, Sigma))
    end
)

Gibbs_s2 = Sampler([:s2],
  (mu, s2, y) ->
    begin
      a = length(y) / 2.0 + shape(s2.distr)
      b = sumabs2(y - mu) / 2.0 + scale(s2.distr)
      rand(InverseGamma(a, b))
    end
)

## User-Defined Sampling Scheme
scheme3 = [Gibbs_beta, Gibbs_s2]
setsamplers!(model, scheme3)

## Initial Values
inits = [Dict{Symbol, Any}(
    :y => dt[:y],
    :beta => [0, 0],
    :s2 => 0.05 #Putting [0.05] wouldn't workt!!!!? Why?
  ) 
  for i in 1:3
]


## MCMC Simulations
sim3 = mcmc(model, dt, inits, 10000, burnin=1000, thin=2, chains=3)
describe(sim3)