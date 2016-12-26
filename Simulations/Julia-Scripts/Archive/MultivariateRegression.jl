using Distributions;
using Mamba;

## Settings
srand(12345);
tbeta = [1 2 3 4
         1 4 6 8];
tsig = [1. 0.1 0.2 0.3
        0.1 1. 0.2 0.3
        0.2 0.2 1. 0.3
        0.3 0.3 0.3 1.];
n=1000;

## Generate Data
dt = Dict{Symbol, Any}(
  :x => hcat(ones(n,1),rand(n))
  ) ;
dt[:y] = dt[:x] * tbeta + (rand(MvNormal(zeros(4), tsig), n))';
dt[:n] = n;

## Auxiliary prior settings, and computations
dt[:nu0] = max(4, 0.01n)
dt[:A]   = dt[:nu0] * diagm(collect(var(dt[:x],1)))
dt[:V0]  = dt[:nu0] * eye(4)
dt[:Bhat] = inv(dt[:x]' * dt[:x]) * dt[:x]' * dt[:y]
dt[:Bbar] = zeros(2,4)
dt[:Btild]= inv(dt[:x]' * dt[:x] + dt[:A]) * (dt[:x]' * dt[:x] * dt[:Bhat] + dt[:A] * dt[:Bbar])
dt[:S] = (dt[:y] - dt[:x] * dt[:Btild])' * (dt[:y] - dt[:x] * dt[:Btild]) + 
        (dt[:Btild]-dt[:Bbar])' * dt[:A] * (dt[:Btild]-dt[:Bbar])

## Setup the model with priors
model = Model(

  beta = Stochastic(1,
    () -> MvNormal(zeros(8), kron(s2, inv(A)))
  ),

  s2 = Stochastic(2, 
    () -> InverseWishart(nu0, V0)
  )

)

## Sampling Scheme
## User-Defined Samplers

Gibbs_beta = Sampler([:beta],
  (beta, s2, x, Btild, A) ->
    begin
      rand(MvNormal(vec(Btild), kron(s2, inv(x' * x + A))))
    end
)

Gibbs_s2 = Sampler([:s2],
  (nu0, n, V0, S) ->
    begin
      rand(InverseWishart(nu0+n, V0+S))
    end
)

## User-Defined Sampling Scheme
scheme3 = [Gibbs_beta, Gibbs_s2]
setsamplers!(model, scheme3)

## Initial Values
inits = [Dict{Symbol, Any}(
    :beta => zeros(8, 1),
    :s2 =>  rand(InverseWishart(dt[:nu0]+dt[:n], dt[:V0]+dt[:S])) #Putting [0.05] wouldn't workt!!!!? Why?
  ) 
  for i in 1:3
]


## MCMC Simulations
sim3 = mcmc(model, dt, inits, 10000, burnin=1000, thin=2, chains=3)
describe(sim3)