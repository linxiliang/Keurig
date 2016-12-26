@everywhere using DataFrames; #For data storage and grouping
@everywhere using Calculus;  #For numerical gradient to check on our analytical gadient
@everywhere using Distributions; #For MCMC distribution draws
#@everywhere importall Mamba; #Not currently used... Engine for MCMC, but doesn't work properly for me.
@everywhere using Optim; #For optimization and obtaining the negative hessian for MH.

## Settings
@everywhere n  = 89913;
@everywhere nh = 991;

# Settings on creating X matrix - the first product must be the outside option!
# 1  3  4  6  7  8  9 10 11 13 14 15 17
@everywhere xnames = [:a1, :a3, :a4, :a6, :a7, :a8, :a9, :a10, :a11, :a13, :a14, :a15, :a17, :price, :size1_amount, :brand_lag, :brand_cum]
#@everywhere xnames = [:a1, :a2, :a3, :a4, :a5, :a6, :a7, :a8, :a9, :a10, :a11, :a12, :a13, 
#:a14, :a15, :a16, :a17, :a18, :a19, :a20, :a21, :a22, :price, :size1_amount, :brand_lag, :brand_cum]

# Number of levels to be estimated
@everywhere np = length(xnames) - 1; # Number of parameters, relevant for estimation.

# The purchase occasion indicator needs to be unique even across individuals.
# purchase occassion need to be stored in ":t"
# hh code need to be stored in ":hh", and need to be incremental from 1.
# :expu is reserved for exponential of utility
# :prob is reserved for choice probability

#------------------------------------------------------------------------------------#

# Define functions

# Negative of the likelihood
@everywhere function ll(b::Vector, df::DataFrame)
  df[:expu] = exp(Matrix(df[xnames]) * [0;b])
  df[:prob] = Array(Float64, size(df,1))
  by(df, :t) do df
    df[:prob] = df[:expu] / sum(df[:expu])
  end
  return -sum(df[:y] .* log(df[:prob]))
end

# Gradient of negative of log-likelihood
@everywhere function llg(b::Vector, df::DataFrame)
  df[:expu] = exp(Matrix(df[xnames]) * [0;b])
  df[:prob] = Array(Float64, size(df,1))
  by(df, :t) do df
    df[:prob] = df[:expu] / sum(df[:expu])
  end

  grd = zeros(Float64, np+1)
  for v in unique(df[:t])
    dft = df[df[:t].==v, :]
    X = Matrix(dft[xnames])
    pvx = dft[:prob] .* X
    grd += (X - repmat(sum(pvx, 1), size(dft)[1]))' * dft[:y]
  end

  return -grd[2:end]
end

# Hessian of negative log-likelihood
@everywhere function llh(b::Vector, df::DataFrame)
  df[:expu] = exp(Matrix(df[xnames]) * [0;b])
  df[:prob] = Array(Float64, size(df,1))
  by(df, :t) do df
    df[:prob] = df[:expu] / sum(df[:expu])
  end

  hess = zeros(Float64, np+1, np+1)
  for v in unique(df[:t])
    dft = df[df[:t].==v, :]
    X = Matrix(dft[xnames])
    pvx = dft[:prob] .* X
    jn = size(dft)[1]
    tm1 = zeros(Float64, np+1, np+1)
    tm2 = zeros(Float64, np+1, np+1)
    for i in 1:jn
       tm1 = tm1 + pvx[i,:]' * X[i, :]
       for j in 1:jn 
           tm2 = tm2 + pvx[i,:]' * pvx[j,:]
       end
    end
    hess += (tm1 - tm2)
  end

  return hess[2:end, 2:end]
end

#---------------------------------------------------------------------------------------------#

# Load Data
df = readtable("Desktop/keurig_julia_samp.csv"); # sample 991 households
# df = readtable("Desktop/keurig_julia.csv"); # Full data, about 8000 households
df[:expu] = 0.0 ;
df[:prob] = 0.0 ;
df[:y] = df[:purchased];

#---------------------------------------------------------------------------------------------#
# Bayesian Estimation 

# MCMC Settings
burnin = 1000
thin   = 20
draws  = 5000
totdraws = draws*thin + burnin

## Auxiliary prior settings, and computations
@everywhere nu0 = max(4, 0.01*nh, np);
@everywhere V0 = nu0 * eye(np);
@everywhere sig0 = rand(InverseWishart(nu0, V0));
@everywhere bhat0 = zeros(Float64, np);
beta0 = SharedArray(Float64, nh, np);
@everywhere Z = ones(nh, 1);
@everywhere A = 0;
@everywhere Dbar = zeros(Float64, 1, np);
@everywhere s2 = 2.93^2/np;
@everywhere w = 0.1;

# Negative Hessian function
@everywhere function H(h::Real, df::DataFrame)
   dft = df[df[:hh].==h,:]
   rho = size(dft,1)/size(df,1)
   # println("no error here!")

   # Find the fractional loglikelihood
   fll(b::Vector) = (1-w)*ll(b, dft) + w*rho*ll(b, df)

   # Obtain the optim of the fractional likelihood
   opt = optimize(fll, zeros(Float64, np))

   # Find the hessian of fractional likelihood
   fllh(b::Vector) = (1-w) * llh(b, dft) + w * rho * llh(b, df)
   #println("no error here! w = $(w), rho = $(rho)")

   # Return the hessian
   return fllh(opt.minimum)
end

# Code up posterior proportions 
@everywhere function pif(h::Real, b0::Vector, b1::Vector, bhat::Vector, sig::Matrix, df::DataFrame)
   dft = df[df[:hh].==h,:]
   return exp(-ll(b1, dft) + log(pdf(MvNormal(bhat, sig), b1)) + ll(b0, dft) - log(pdf(MvNormal(bhat, sig), b0)))
end

# Obtain the hessian for each consumer's fractional likelihood
Hess = SharedArray(Float64, np, np, nh);
@sync @parallel for h in 1:nh
    Hess[:,:,h] = convert(Array, H(h, df))
end

#Initialize storage of MCMC draws
bhatd = zeros(Float64, draws, np);
sigd = zeros(Float64, np, np, draws);
beta1 = SharedArray(Float64, nh, np);

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
  end

  # RW MH to draw betas for each household
  # Do the per consumer draw in parallel
  @sync @parallel for i in 1:nh
     #println(ll(zeros(Float64, np), df))
     # Scaling using s2
     cov_i = s2 * inv(Hess[:,:,i] + inv(sig))
     beta1[i,:] = beta0[i,:] + rand(MvNormal(zeros(Float64, np), cov_i))'
     alpha = min(1, pif(i,beta0[i, :]'[:,1],beta1[i, :]'[:,1], bhat, sig, df))
     #println("This is $i")
     if rand() <= alpha 
        beta0[i, :] = beta1[i, :]
     end
  end

  # Indicate the progress
  println("Finished drawing $(d) out of $(totdraws)")
end

finalize(workers)
