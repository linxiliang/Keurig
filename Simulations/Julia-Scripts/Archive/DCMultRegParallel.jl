@everywhere using DataFrames; #For data storage and grouping
@everywhere using Calculus;  #For numerical gradient to check on our analytical gadient
@everywhere using Distributions; #For MCMC distribution draws
#@everywhere importall Mamba; #Not currently used... Engine for MCMC, but doesn't work properly for me.
@everywhere using Optim; #For optimization and obtaining the negative hessian for MH.

## Settings
@everywhere srand(12345);
@everywhere tbeta = [1.0, 1.0, -2.0];
@everywhere tsig = [1. 0. 0.
        0. 1. 0.
        0. 0. 1.];
@everywhere n  = 1000;
@everywhere nh = 100;

# Simulate Preferences
@everywhere tbetad = rand(MvNormal(tbeta, tsig), nh)';
@everywhere tprefer=DataFrame(hh = collect(1:nh), b0 = zeros(Float64, nh), b1 =tbetad[:,1], b2 = tbetad[:,2], b3 = tbetad[:,3]);

# Data for Simple Discrete Choice
@everywhere df = DataFrame(hh = zeros(Int64, 3n), t = repmat(1:(n), 3), 
               a0 = [ones(n); zeros(2n)], 
               a1= [zeros(n); ones(n); zeros(n)],
               a2= [zeros(2n); ones(n)],
               price = [zeros(n); rand(2n)]);

@everywhere df[:hh] = ceil(Int64, df[:t]/10);

# Join Back Preferences
@everywhere df = join(df, tprefer, on = :hh);

# Sort the data by t choice occasions.
@everywhere sort!(df, cols = [:t]);

# Simulate latent utility draws
@everywhere df[:eps]= -log(-log(rand(3000)));

# Compute the utilities
@everywhere df[:util] = sum(hcat(df[:a0], df[:a1], df[:a2], df[:price]) .* 
                hcat(df[:b0], df[:b1], df[:b2], df[:b3]), 2)[:,1] + df[:eps];

# Determine the choices by consumers
@everywhere df[:y]  = zeros(Int64, 3n);
@everywhere by(df, :t) do df
    df[:y] = df[:util] .== repmat([maximum(df[:util])], length(df[:util]))
end;

# Define the negative of log likelihood function
@everywhere df[:expu] = 0.;
@everywhere df[:prob] = 0.;

@everywhere function ll(b::Vector, df::DataFrame)
  df[:expu] = exp(hcat(df[:a0], df[:a1], df[:a2], df[:price]) * [0;b])
  df[:prob] = Array(Float64, size(df,1))
  by(df, :t) do df
    df[:prob] = df[:expu] / sum(df[:expu])
  end
  return -sum(df[:y] .* log(df[:prob]))
end

# Gradient of negative of log-likelihood
@everywhere function llg(b::Vector, df::DataFrame)
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
@everywhere function llh(b::Vector, df::DataFrame)
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
burnin = 2000
thin   = 50
draws = 1000
totdraws = draws*thin + burnin

## Auxiliary prior settings, and computations
@everywhere nu0 = max(4, 0.01*nh)
@everywhere V0 = nu0 * eye(3)
@everywhere sig0 = rand(InverseWishart(nu0, V0))
@everywhere bhat0 = zeros(3)
beta0 = SharedArray(Float64, nh, 3)
@everywhere Z = ones(nh, 1)
@everywhere A = 0;
@everywhere Dbar = [0 0 0];
@everywhere s2 = 2.93^2/3;
@everywhere w = 0.1;

# Negative Hessian function
@everywhere function H(h::Real, df::DataFrame)
   dft = df[df[:hh].==h,:]
   rho = size(dft,1)/size(df,1)
   # println("no error here!")

   # Find the fractional loglikelihood
   fll(b::Vector) = (1-w)*ll(b, dft) + w*rho*ll(b, df)

   # Obtain the optim of the fractional likelihood
   opt = optimize(fll, [0.0,0.0,0.0])

   # Find the hessian of fractional likelihood
   fllh(b::Vector) = (1-w) * llh(b, dft) + w * rho * llh(b, df)
   #println("no error here! w = $(w), rho = $(rho)")

   # Return the hessian
   return fllh(opt.minimum)
end

# Code up posterior proportions 
@everywhere function pif(h::Real, b0::Vector, b1::Vector, bhat::Vector, sig::Matrix, df::DataFrame)
   dft = df[df[:hh].==h,:]
   return (ll(b1, dft) * pdf(MvNormal(bhat, sig), b1))/(ll(b0, dft) * pdf(MvNormal(bhat, sig), b0))
end

# Obtain the hessian for each consumer's fractional likelihood
Hess = SharedArray(Float64, 3, 3, nh)
@sync @parallel for h in 1:nh
    Hess[:,:,h] = convert(Array, H(h, df))
end

#Initialize storage of MCMC draws
bhatd = zeros(Float64, draws, 3)
sigd = zeros(Float64, 3, 3, draws)
beta1 = SharedArray(Float64, nh, 3)

## Gibbs Sampling
for d=1:totdraws
  # For given betas - preference matrix, draw posterior mean and covariance matrix
  # Compute the posterior mean and variance of betas
  Dhat = inv(Z'*Z)*Z'*beta0
  Dtild = inv(Z'*Z+A)*(Z'*Z*Dhat + A*Dbar)
  S    = (beta0 - Z*Dtild)' * (beta0 - Z*Dtild) + (Dhat - Dbar)' * A * (Dhat - Dbar)
   
  # Draw from the posterior 
  sig  = rand(InverseWishart(nu0+nh, V0+S))
  bhat = rand(MvNormal(vec(Dhat), sig))

  # Store the posterior draws of mean betas and sigmas
  if ((d > burnin) && (d % thin == 1))
    indx = ceil(Int64, (d - burnin)/thin) ;
    bhatd[indx, :] = vec(Dhat)'/sqrt(sig[1,1]); 
    sigd[:, :, indx] = sig/sig[1,1];
  end

  # RW MH to draw betas for each household
  # Do the per consumer draw in parallel
  @sync @parallel for i in 1:nh
     #println(ll([0.0, 0.0, 0.0], df))
     cov_i = s2 * inv(Hess[:,:,i] + inv(sig))
     beta1[i,:] = beta0[i,:] + rand(MvNormal([0.0,0.0,0.0], cov_i))'
     alpha = min(1, pif(i,beta1[i, :]'[:,1],beta0[i, :]'[:,1], bhat, sig, df))
     #println("This is $i")
     if rand() <= alpha 
        beta0[i, :] = beta1[i, :]
     end
  end
end

finalize(workers)