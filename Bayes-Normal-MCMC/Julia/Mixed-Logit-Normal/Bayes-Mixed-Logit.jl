#---------------------------------------------------------------------------------------------#
#
# Bayesian Estimation of Mixed Logit
# Xiliang Lin
# Jan 2016
#---------------------------------------------------------------------------------------------#

# Set working directory
@everywhere cd("$(homedir())/Keurig")
@everywhere workspace() # Clear workspace

@everywhere using DataFrames #For data storage and grouping
@everywhere using Calculus #For numerical gradient to check on our analytical gadient
@everywhere using Distributions #For MCMC distribution draws
@everywhere using Optim #For optimization and obtaining the negative hessian for MH.
#------------------------------------------------------------------------------------#

# Data Loading and Processing

# Load Data
@everywhere df = readtable("Data/MLogit-Data/keurig_julia.csv"); # Full data, about 8000 households

# Set the names of variables
@everywhere prod_name = :product
@everywhere trip_name = :t
@everywhere purch_name = :purchased
@everywhere avail_name = :avail

# Treat state variable
@everywhere df[:brand_cum] = log(df[:brand_cum]+1)

# Settings on creating X matrix - the first product must be the outside option!
@everywhere xnames = [:a1, :a2, :a3, :a4, :a5, :a6, :a7, :a8, :a9, :a10, :a11,
                      :a12, :a13, :a14, :a15, :a16, :a17,
                      :price, :size1_amount, :brand_lag, :brand_cum]

# Extract useful counts
@everywhere np = length(xnames) - 1; # Number of parameters, relevant for estimation.
@everywhere nh = length(unique(df[:hh])); # Number of households
@everywhere nt = length(unique(df[:t])); # Number of choice occassions
@everywhere nprod = length(unique(df[:product])); # Number of product (not uniquely defined)
@everywhere n  = nt*nprod; # Total number of observations

#------------------------------------------------------------------------------------#

# Create data dictionary
# Matrix mulitplication of the same type of entries (e.g. float64) are extremely fast
# Matrix elementwise operatoin with integers are slightly faster than float.
# So convert all X variables to Float64,
@everywhere for (xv in xnames)
   df[xv] = map(Float64, Array(df[xv]))
end

@everywhere dt = Dict{Symbol, Any}(
  :X => sparse(Array(df[:,xnames])),
  :CX => sparse(Array(df[df[purch_name].==1, xnames])),
  :Y => sparse(Array(unstack(df, trip_name, prod_name, purch_name)[:,2:end])),
  :Avail => sparse(Array(unstack(df, trip_name, prod_name, :avail)[:,2:end]))
)

# Find start and end position of each household
@everywhere hhtab = zeros(nh, 4)
@everywhere k = 1
@everywhere for (i, h) in enumerate(unique(df[:hh]))
   hhtab[i,1] = k
   hhtab[i,2] = sum(df[:hh].==h) + k - 1
   hhtab[i,3] = (hhtab[i,1]-1)/nprod + 1
   hhtab[i,4] = hhtab[i,2]/nprod
   k = hhtab[i,2] + 1
   #println("Finished $i out of $(nh).")
end

@everywhere hhtab = map(Int64, hhtab)

@everywhere dth = Dict{Int64, Any}()
@everywhere for (i,h) in enumerate(unique(df[:hh]))
  dth[h] = Dict(:X => dt[:X][hhtab[i,1]:hhtab[i,2], :],
  :CX => dt[:CX][hhtab[i,3]:hhtab[i,4], :],
  :Y => dt[:Y][hhtab[i,3]:hhtab[i,4], :],
  :Avail => dt[:Avail][hhtab[i,3]:hhtab[i,4], :])
  #println("Finished $i out of $(nh).")
end

#------------------------------------------------------------------------------------#

# Define functions
# Negative of the likelihood
# Vectorized version
@everywhere function ll(b::Vector, dt::Dict{Symbol, Any})
  ntt = size(dt[:Y],1)
  expu = exp(sparse(reshape((dt[:X] * [0.;b]), nprod, ntt)')) .* dt[:Avail]
  prob = sum(expu .* dt[:Y], 2) ./ sum(expu, 2)
  #println(b)
  #println(sum(log(prob)))
  return -sum(log(prob))
end

# Gradient of negative of log-likelihood
# Vectorized version
@everywhere function llg(b::Vector, dt::Dict{Symbol, Any})
  ntt = size(dt[:Y],1)
  expu = exp(sparse(reshape((dt[:X] * [0.;b]), nprod, ntt)')) .* dt[:Avail]
  prob = reshape(sparse(expu ./ sum(expu, 2))', ntt * nprod, 1)
  WXprob = reshape(sum(reshape(full(broadcast(*, prob, dt[:X])'), (np+1), nprod, ntt), 2), (np+1), ntt)'
  return reshape(-sum(dt[:CX] - WXprob, 1)[:,2:end], np)
end

# Hessian of negative log-likelihood
# Vectorized version
@everywhere function llh(b::Vector, dt::Dict{Symbol, Any})
  ntt = size(dt[:Y], 1)
  tn = size(dt[:X], 1)
  expu = exp(sparse(reshape((dt[:X] * [0.;b]), nprod, ntt)')) .* dt[:Avail]
  prob = reshape(sparse(expu ./ sum(expu, 2))', ntt * nprod, 1)
  XProb = sparse(broadcast(*, prob, dt[:X]))
  XRep1 = reshape(kron(ones(1, nprod), sparse(XProb))', (np+1), nprod*tn)
  XRep2 = reshape(kron(ones(nprod), reshape(XProb', nprod*(np+1), ntt)), (np+1), tn*nprod)'
  Hess = XProb' * dt[:X] - XRep1 * XRep2
  return full(Hess[2:end,2:end])
end

# Negative Hessian function
@everywhere function H(h::Real, startvalue::Vector)
   rho = size(dth[h][:Y],1)/size(dt[:Y],1)

   # Find the fractional loglikelihood
   fll(b::Vector) = (1-w)*ll(b, dth[h]) + w*rho*ll(b, dt)
   fllg!(b::Vector, storage::Vector) = (storage[:] = (1-w)*llg(b, dth[h]) + w*rho*llg(b, dt))

   # Obtain the optim of the fractional likelihood
   opt = optimize(fll, fllg!, startvalue, method = :bfgs, grtol=1e-10, ftol = 1e-32)

   # Find the hessian of fractional likelihood
   fllh(b::Vector) = (1-w) * llh(b, dth[h]) + w * rho * llh(b, dt)
   #println("no error here! w = $(w), rho = $(rho)")

   # Return the hessian
   return fllh(opt.minimum)
end

# Code up posterior proportions
@everywhere function pif(h::Real, b0::Vector, b1::Vector, bhat::Vector, sig::Matrix)
   return exp(-ll(b1, dth[h]) + log(pdf(MvNormal(bhat, sig), b1)) + ll(b0, dth[h]) - log(pdf(MvNormal(bhat, sig), b0)))
end

#---------------------------------------------------------------------------------------------#

# Optimization for simple logit demand - use as starting point for other computations.
@everywhere b = zeros(Float64, np)
@everywhere llx(b::Vector) = ll(b, dt)
@everywhere llg!(b::Vector, storage::Vector) = (storage[:] = llg(b,dt))
@everywhere llh!(b::Vector, storage::Matrix) = (storage[:] = llh(b,dt))
@everywhere kopt = optimize(llx, llg!, zeros(Float64, np), method = :bfgs, grtol=1e-10, ftol = 1e-32)
#---------------------------------------------------------------------------------------------#
# MCMC Settings
burnin = 10000
thin   = 20
draws  = 5000
totdraws = draws*thin + burnin

# Auxiliary prior settings, and computations
@everywhere nu0 = max(4, 0.01*nh);
@everywhere V0 = nu0 * eye(np);
@everywhere sig0 = rand(InverseWishart(nu0, V0));
@everywhere bhat0 = zeros(Float64, np);
beta0 = SharedArray(Float64, nh, np);
@everywhere Z = ones(nh, 1);
@everywhere A = 0;
@everywhere Dbar = zeros(Float64, 1, np);
@everywhere s2 = 2.93^2/np;
@everywhere w = 0.1;

# Obtain the hessian for each consumer's fractional likelihood
Hess = SharedArray(Float64, np, np, nh);
if (is.file("Data/MLogit-Data/Hessian.csv"))
   temp = readdlm("Data/MLogit-Data/Hessian.csv") # Read the hessian instead of computing it.
   Hess[:] = reshape(temp, np, np, nh)
else
   @sync @parallel for h in 1:nh
       Hess[:,:,h] = H(h, kopt.minimum)
   end
   writedlm("Data/MLogit-Data/Hessian.csv", reshape(Hess, np, np*nh)) # Write the Hessian to a file
end

#Initialize storage of MCMC draws
bhatd = zeros(Float64, draws, np);
sigd = zeros(Float64, np, np, draws);
bhd = zeros(Float64, nh, np, draws);

## Gibbs Sampling
for d=1:totdraws
  # For given betas - preference matrix, draw posterior mean and covariance matrix
  # Compute the posterior mean and variance of betas
  Dhat = inv(Z'*Z)*Z'*beta0
  Dtild = inv(Z'*Z+A)*(Z'*Z*Dhat + A*Dbar)
  S    = (beta0 - Z*Dtild)' * (beta0 - Z*Dtild) + (Dhat - Dbar)' * A * (Dhat - Dbar)

  # Draw from the posterior
  sig  = rand(InverseWishart(nu0+nh, V0+S))
  bhat = rand(MvNormal(vec(Dhat), kron(sig, inv(Z'*Z))))

  # Store the posterior draws of mean betas and sigmas
  if ((d > burnin) && (d % thin == 0))
    indx = ceil(Int64, (d - burnin)/thin) ;
    bhatd[indx, :] = bhat;
    sigd[:, :, indx] = sig;
    bhd[:, :, indx] = beta0;
  end

  # RW MH to draw betas for each household
  # Do the per consumer draw in parallel
  @sync @parallel for i in 1:nh
     #println(ll(zeros(Float64, np), df))
     # Scaling using s2
     cov_i = s2 * inv(Hess[:,:,i] + inv(sig))
     beta1 = beta0[i,:] + rand(MvNormal(zeros(Float64, np), cov_i))'
     alpha = min(1, pif(i,vec(beta0[i, :]),vec(beta1), bhat, sig))
     #println("This is $i")
     if rand() <= alpha
        beta0[i, :] = beta1
     end
  end

  # Indicate the progress
  println("Finished drawing $(d) out of $(totdraws)")
end

writedlm("Data/MCMC-Draws/coef.csv", bhatd) # Write the posterior average coefficients
writedlm("Data/MCMC-Draws/sig.csv", mean(sigd, 3)[:,:,1]) # Write the posterior sigma
writedlm("Data/MCMC-Draws/i_coef.csv", mean(bhd, 3)[:,:,1]) # Write the posterior sigma

finalize(workers)
