using DataFrames; #For data storage and grouping
using Calculus;  #For numerical gradient to check on our analytical gadient
using Distributions; #For MCMC distribution draws
using Mamba; #Not currently used... Engine for MCMC, but doesn't work properly for me.

## Settings
srand(12345);
tbeta = [1,1,-2];
tsig = [1. 0.2 0.1
        0.2 1. 0.1
        0.1 0.1 0.9];
n  = 1000;
nh = 100;

# Simulate Preferences
tbetad = rand(MvNormal(tbeta, tsig), nh)';
tprefer=DataFrame(hh = [1:nh], b0 = zeros(Float64, nh), b1 =tbetad[:,1], b2 = tbetad[:,2], b3 = tbetad[:,3])

# Data for Simple Discrete Choice
df = DataFrame(hh = zeros(Int64, 3n), t = repmat(1:(n), 3), 
               a0 = [ones(n),zeros(2n)], 
               a1= [zeros(n), ones(n), zeros(n)],
               a2= [zeros(2n), ones(n)],
               price = [zeros(n), rand(2n)])

df[:hh] = ceil(Int64, df[:t]/10);

# Join Back Preferences
df = join(df, tprefer, on = :hh)

# Sort the data by t choice occasions.
sort!(df, cols = [:t]);

# Simulate latent utility draws
df[:eps]= -log(-log(rand(3000)));

# Compute the utilities
df[:util] = sum(hcat(df[:a0], df[:a1], df[:a2], df[:price]) .* 
                hcat(df[:b0], df[:b1], df[:b2], df[:b3]), 2)[:,1] + df[:eps];

# Determine the choices by consumers
df[:y]  = zeros(Int64, 3n);
by(df, :t) do df
    df[:y] = df[:util] .== repmat([maximum(df[:util])], length(df[:util]))
end;

# Define the negative of log likelihood function
df[:expu] = 0.;
df[:prob] = 0.;
function ll(b::Vector, df::DataFrame)
  df[:expu] = exp(hcat(df[:a0], df[:a1], df[:a2], df[:price]) * [0;b])
  df[:prob] = Array(Float64, size(df,1))
  by(df, :t) do df
    df[:prob] = df[:expu] / sum(df[:expu])
  end
  return -sum(df[:y] .* log(df[:prob]))
end

# Gradient of negative of log-likelihood
function llg(b::Vector, df::DataFrame)
  df[:expu] = exp(hcat(df[:a0], df[:a1], df[:a2], df[:price]) * [0;b])
  df[:prob] = Array(Float64, size(df)[1])
  by(df, :t) do df
    df[:prob] = df[:expu] / sum(df[:expu])
  end

  grd = zeros(Float64, length(b)+1)
  for v in unique(df[:t])
    dft = df[df[:t].==v, :]
    X = hcat(dft[:a0], dft[:a1], dft[:a2], dft[:price])
    pvx = dft[:prob] .* X
    grd += (X - repmat(sum(pvx, 1), size(dft)[1]))' * dft[:y]
  end

  return -grd[2:4]
end

function llg(b::Vector, df::DataFrame)
  df[:expu] = exp(hcat(df[:a0], df[:a1], df[:a2], df[:price]) * [0;b])
  df[:prob] = Array(Float64, size(df)[1])
  by(df, :t) do df
    df[:prob] = df[:expu] / sum(df[:expu])
  end

  grd = zeros(Float64, length(b)+1)
  for v in unique(df[:t])
    dft = df[df[:t].==v, :]
    X = hcat(dft[:a0], dft[:a1], dft[:a2], dft[:price])
    pvx = dft[:prob] .* X
    grd += (X - repmat(sum(pvx, 1), size(dft)[1]))' * dft[:y]
  end

  return -grd[2:4]
end

# Hessian of negative log-likelihood
function llh(b::Vector, df::DataFrame)
  df[:expu] = exp(hcat(df[:a0], df[:a1], df[:a2], df[:price]) * [0;b])
  df[:prob] = Array(Float64, size(df)[1])
  by(df, :t) do df
    df[:prob] = df[:expu] / sum(df[:expu])
  end

  hess = zeros(Float64, length(b)+1, length(b)+1)
  for v in unique(df[:t])
    dft = df[df[:t].==v, :]
    X = hcat(dft[:a0], dft[:a1], dft[:a2], dft[:price])
    pvx = dft[:prob] .* X
    jn = size(dft)[1]
    tm1 = zeros(Float64, length(b)+1, length(b)+1)
    tm2 = zeros(Float64, length(b)+1, length(b)+1)
    for i in 1:jn
       tm1 = tm1 + pvx[i,:]' * X[i, :]
       for j in 1:jn 
           tm2 = tm2 + pvx[i,:]' * pvx[j,:]
       end
    end
    hess += (tm1 - tm2)
  end

  return hess[2:(length(b)+1), 2:(length(b)+1)]
end

#---------------------------------------------------------------------------------------------#

# MCMC Settings
burnin = 5000
thin   = 5
chains = 50000

## Auxiliary prior settings, and computations
nu0 = max(4, 0.01*nh)
V0 = nu0 * eye(3)
sig0 = rand(InverseWishart(nu0, V0))
bhat0 = zeros(3)
beta0 = rand(MvNormal(bhat0, sig0), 100)'
Z = ones(nh, 1)
A = 0;
Dbar = [0 0 0];
s2 = 2.93^2/3;
w = 0.1;

# Negative Hessian function
function H(h::Real, df::DataFrame)
   dft = df[df[:hh].==h,:]
   rho = size(dft,1)/size(df,1)
   # Find the fractional loglikelihood
   fll(b::Vector) = (1-w)*ll(b, dft) + w*rho*ll(b, df)
   
   # Obtain the optim of the fractional likelihood
   opt = optimize(fll, [0.0,0.0,0.0])

   # Find the hessian of fractional likelihood
   fllh(b::Vector) = (1-w) * llh(b, dft) + w * rho * llh(b, df)

   # Return the hessian
   return fllh(opt.minimum)
end

# Code up posterior proportions 
function pif(h::Real, b0::Vector, b1::Vector, bhat::Vector, sig::Matrix)
   dft = df[df[:hh].==h,:]
   return (ll(b1, dft) * pdf(MvNormal(bhat, sig), b1))/(ll(b0, dft) * pdf(MvNormal(bhat, sig), b0))
end


#Initialize storage of MCMC draws
bhatd = SharedArray(zeros(Float64, nh, 3)
sigd = zeros(Float64, 3, 3, nh)
beta1 = rand(MvNormal(bhat0, sig0), 100)'

## Gibbs Sampling
for d=1:100
  # For given betas - preference matrix, draw posterior mean and covariance matrix
  # Compute the posterior mean and variance of betas
  Dhat = inv(Z'*Z)*Z'*beta0
  Dtild = inv(Z'*Z+A)*(Z'*Z*Dhat + A*Dbar)
  S    = (beta0 - Z*Dtild)' * (beta0 - Z*Dtild) + (Dhat - Dbar)' * A * (Dhat - Dbar)
   
  # Draw from the posterior 
  sig  = rand(InverseWishart(nu0+nh, V0+S))
  bhat = rand(MvNormal(vec(Dhat), sig))

  # RW MH to draw betas for each household
  # Do the per consumer draw in parallel
  a = SharedArray(Float64,10)
  @parallel for i=1:10
    a[i] = i
  end
  for i in 1:nh
     cov_i = s2 * inv(convert(Array, H(i,df)) + inv(sig))
     beta1[i,:] = beta0[i,:] + rand(MvNormal([0,0,0], cov_i))'
     alpha = min(1, pif(i,beta1[i, :]'[:,1],beta0[i, :]'[:,1], bhat, sig))
     if rand() <= alpha 
        beta0[i, :] = beta1[i, :]
     end
  end 
end
