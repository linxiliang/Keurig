##########################################################################################
#
# Dynamic Adoption Problem - using IJC Algorithm
# Xiliang Lin
# Jan 2016
#
##########################################################################################
# Settings
cd("$(homedir())/Keurig");

#Call simple computation Dynamic Adoption Algorithm
#seed the random number generator
srand(12345);
using Distributions;
include("Scripts/Bayes-Normal-MCMC/Julia/BellmanFun.jl");

# True Parameters
discountfactor = 0.975
coef = [-10, 0.05]
tol  = 1e-16

##########################################################################################
# Simulate availability

#Maximum number of products available
nprod = 10
tbetas = vcat(5*rand(nprod), -5, 1, -0.5);
include("Scripts/Bayes-Normal-MCMC/Julia/ValueSimulation.jl");

# Define the transition probability
tprob = Array(Float64, 2, 2, nprod);
for i = 1:nprod
  tprob[:,:,i] = [0.99 0.01;
                  0.01 0.995]
end

# Number of periods
T = 200

# Start with only brand 10 available.
av = zeros(Float64, T, nprod);
av[1, 10] = 1.0;

# Simulate Availability
for (t in 2:T)
  for (j in 1:nprod)
    idx = Int64(av[t-1, j])
    rx= rand()
    println("prob is $(tprob[idx+1, idx+1, j]), and rand is $(rx)")
    if rx > tprob[idx+1, idx+1, j]
      av[t, j] = ifelse(idx==0, 1, 0)
    else
      av[t, j] = idx
    end
  end
end

# Compute the delta for each occasion
delta = [deltaf(vec(av[i,:])) for i in 1:T]
delta = map(Float64, delta)

#----------------------------------------------------------------------------------------#
# Price Process
prices = [1.29, 1.19, 1.09] ;
prices_p = [0.85, 0.10, 0.05] ;
delta = reshape(repmat(delta', length(prices)), length(prices)*T)

# Simulate prices
pdistr = Multinomial(1, prices_p)
pvec = repmat(prices, 1, T)[BitArray(rand(pdistr, T))]

#----------------------------------------------------------------------------------------#
# Intialize a function to start the iteration
w0 = zeros(Float64, T, length(prices))
w1 = zeros(Float64, T, length(prices))
eps_diff = 1.
k = 0
while(eps_diff > tol)
  w1[:] = w(w0, prices, prices_p)
  eps_diff = sum(abs(w1 - w0))
  w0[:] = w1
  println("The current epsilon is $(eps_diff)")
  k+=1
  println("The current iteration is $k") # About 500 iterations to achieve convergence.
end

# Compute the conditional choice probabilities
(nr, nc) = size(w0)
zvec = zeros(Float64, nc*nr)
cont_value = hcat(zeros(Float64, nr), discountfactor .* sum(w0 .* repmat(prices_p', nr), 2))
cont_value = reshape(repmat(cont_value', nc), 2, Int64(length(cont_value) * nc/2))'
current_util = hcat(hcat(repmat(prices, nr), delta) * coef, zvec)
v =  current_util + cont_value
ccp = exp(v)./sum(exp(v),2)

#----------------------------------------------------------------------------------------#
# Give the CCP, simulate choices - the current model has no identification!
# Too little variation in prices - we either need arrival of new customers or something else!
msize = 10000
dt = Dict{Symbol, Any}();
dt[:id] = reshape(repmat(collect(1:msize)', T), msize*T)
dt[:t] = repmat(collect(1:T), msize)
dt[:price] = repmat(pvec, msize)
adoption = ones(Float64, msize*T)
astate = ones(Float64, msize*T)
for i in 1:msize
    adpt = 0
    for t in 1:T
      px = findfirst(prices, pvec[t])
      indt = px + nc * (t-1)
      inda = (i-1)*T + t
      adoption[inda] = 0
      astate[inda] = 0
      if rand()<ccp[indt,1]
        adoption[inda] = 1
        break;
      end
    end
end
dt[:adoption] = adoption
dt[:astate] = astate

#----------------------------------------------------------------------------------------#
# Estimation using IJC

# Settings
N = 1000 # Number of lags to store and use for value function approximation.
include("Scripts/Bayes-Normal-MCMC/Julia/WApprox-LL.jl"); # Include the script computing WApproximation and LL.

# Code up posterior proportions
function postd(ccp0::Matrix, ccp1::Matrix, theta0::Vector, theta1::Vector, bhat::Vector, sigb::Matrix)
   return exp(LLF(ccp1) + log(pdf(MvNormal(bhat, sigb), theta1)) - LLF(ccp0) - log(pdf(MvNormal(bhat, sigb), theta0)))
end

#----------------------------------------------------------------------------------------#
# MCMC Settings
burnin = 3000
thin   = 20
draws  = 1000
totdraws = draws*thin + burnin

# Priors
bhat = zeros(Float64, 2)
sigb = eye(2)*1000

# Let it be a random walk MH out loop
theta0 = zeros(2)
#theta0[2] = 0.05
# Let the proposal variance to be small
sigs = [0.1 0;
        0 0.001]
walkdistr = MvNormal(zeros(2), sigs)

# Initialize storage for proposal parameters and corresponding w
thtild = zeros(Float64, 2, N)
wtild = zeros(Float64, length(prices), T, N)
w0 =  WApprox(theta0, sigs, thtild, wtild) # Approximate the new w
ccp0 = cppf!(w0, prices, prices_p, theta0)

# Initialize storage of MCMC draws
thatd = zeros(Float64, draws, 2);

# Let the bandwidth to be very small
bsigs = [0.001 0;
        0 0.00001]

## MCMC Draws
for d=1:totdraws
  theta1 = theta0 + vec(rand(walkdistr, 1))
  #theta1[2] = 0.05
  w1 = WApprox(theta1, bsigs, thtild, wtild) # Approximate the new w
  ccp1 = cppf!(w1, prices, prices_p, theta1)
  wtild[:, :, 2:end] = wtild[:,:, 1:(end-1)]
  wtild[:, :, 1] = w1'
  thtild[:, 2:end] = thtild[:, 1:(end-1)]
  thtild[:, 1] = theta1

  alpha = min(1, postd(ccp0, ccp1, theta0, theta1, bhat, sigb))
  if rand() <= alpha
      theta0 = theta1
      w0 = w1
      ccp0 = ccp1
  end

  # Store the posterior draws of mean betas and sigmas
  if ((d > burnin) && (d % thin == 0))
    indx = ceil(Int64, (d - burnin)/thin) ;
    thatd[indx, :] = theta0;
  end

  # Indicate the progress
  println("Finished drawing $(d) out of $(totdraws)")
end

/home/xlin0/Keurig/Scripts/Two-Step-Demand/Machine-Adoption/BellmanFun.jl
