##########################################################################################
#
# Simple Dynamic Adoption Problem - Simulation
# Xiliang Lin
# Jan 2016
#
##########################################################################################
# Settings
cd("$(homedir())/Keurig");

# Load Packages
using Distributions;
using Optim;
include("Scripts/Bayes-Normal-MCMC/Julia/BellmanFun.jl");

# Parameters
discountfactor = 0.95
coef = [-1.5, 0.65]
tol  = 1e-16
#----------------------------------------------------------------------------------------#
# Price Process
prices = [129, 119, 109] ;
prices_p = [0.85, 0.10, 0.05] ;

# Set maximum of available products and preference over them.
nprod = 10;
tbetas = vcat(5*rand(nprod), -5, 1, -0.5);
include("Scripts/Bayes-Normal-MCMC/Julia/ValueSimulation.jl");
#----------------------------------------------------------------------------------------#
# Intialize a function to start the iteration
w0 = zeros(Float64, nprod, length(prices))
w1 = zeros(Float64, nprod, length(prices))
delta = zeros(Float64, nprod)
# Solve the problem using value function iteration of w
for (i in 1:nprod)
  av  = zeros(Float64, nprod)
  av[i] = 1.0
  delta[i] = deltaf(av)
end

delta = reshape(repmat(delta', length(prices)), length(prices)*nprod)

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
zvec = zeros(Float64, length(prices)*nprod)
cont_value = hcat(zeros(Float64, nprod), discountfactor .* sum(w0 .* repmat(prices_p', nprod), 2))
cont_value = reshape(repmat(cont_value', length(prices)), 2, Int64(length(cont_value) * length(prices)/2))'
current_util = hcat(hcat(repmat(prices, nprod), delta) * coef, zvec)

v =  current_util + cont_value
ccp = exp(v)./sum(exp(v),2)
ccp
