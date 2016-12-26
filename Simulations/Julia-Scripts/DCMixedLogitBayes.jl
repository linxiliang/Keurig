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
@everywhere nh = 200;
@everywhere nt = 4000;
@everywhere nprod = 3;
@everywhere n  = nt*nprod;

#------------------------------------------------------------------------------------#

# Simulation part
# Simulate Preferences
@everywhere tbetad = rand(MvNormal(tbeta, tsig), nh)';
@everywhere tprefer=DataFrame(hh = collect(1:nh), b0 = zeros(Float64, nh), b1 =tbetad[:,1], b2 = tbetad[:,2], b3 = tbetad[:,3]);

# Data for Simple Discrete Choice
@everywhere df = DataFrame(hh = zeros(Int64, 3*nt), t = repmat(1:(nt), 3),
               product = repmat(1:3, nt),
               avail = ones(n),
               a0 = [ones(Float64, nt); zeros(Float64,2*nt)], 
               a1= [zeros(Float64, nt); ones(Float64, nt); zeros(Float64, nt)],
               a2= [zeros(Float64, 2*nt); ones(Float64, nt)],
               price = [zeros(Float64, nt); rand(2*nt)]);

@everywhere df[:hh] = ceil(Int64, df[:t]/(nt/nh));

# Join Back Preferences
@everywhere df = join(df, tprefer, on = :hh);

# Sort the data by t choice occasions.
@everywhere sort!(df, cols = [:t, :product]);

# Simulate latent utility draws
@everywhere df[:eps]= -log(-log(rand(n)));

# Compute the utilities
@everywhere df[:util] = sum(hcat(df[:a0], df[:a1], df[:a2], df[:price]) .* 
                hcat(df[:b0], df[:b1], df[:b2], df[:b3]), 2)[:,1] + df[:eps];

# Determine the choices by consumers
@everywhere df[:purchased]  = zeros(Int64, n);
@everywhere by(df, :t) do df
    df[:purchased] = df[:util] .== repmat([maximum(df[:util])], length(df[:util]))
end;

# Define the negative of log likelihood function
@everywhere df[:expu] = 0.;
@everywhere df[:prob] = 0.;

#------------------------------------------------------------------------------------#

# Create data dictionary 
xname = [:a0, :a1, :a2, :price]
dt = Dict{Symbol, Any}(
  :X => sparse(Array(df[:,xname])),
  :CX => sparse(Array(df[df[:purchased].==1, xname])),
  :Y => sparse(Array(unstack(df, :t, :product, :purchased)[:,2:end])),
  :Avail => sparse(Array(unstack(df, :t, :product, :avail)[:,2:end]))
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

# Split combine version
@everywhere function ll(b::Vector, df::DataFrame)
  df[:expu] = exp(hcat(df[:a0], df[:a1], df[:a2], df[:price]) * [0;b])
  df[:prob] = Array(Float64, size(df,1))
  by(df, :t) do df
    df[:prob] = df[:expu] / sum(df[:expu])
  end
  return -sum(df[:y] .* log(df[:prob]))
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

# Loop version - much slower
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

# Loop version
@everywhere function llh(b::Vector, df::DataFrame)
  df[:expu] = exp(hcat(df[:a0], df[:a1], df[:a2], df[:price]) * [0;b])
  df[:prob] = Array(Float64, size(df)[1])
  by(df, :t) do df
    df[:prob] = df[:expu] / sum(df[:expu])
  end

  tm1 = zeros(Float64, length(b)+1, length(b)+1)
  tm2 = zeros(Float64, length(b)+1, length(b)+1)

  for v in unique(df[:t])
    dft = df[df[:t].==v, :]
    X = hcat(dft[:a0], dft[:a1], dft[:a2], dft[:price])
    pvx = dft[:prob] .* X
    jn = size(dft)[1]
    for i in 1:jn
       tm1 = tm1 + pvx[i,:]' * X[i, :]
       for j in 1:jn 
           tm2 = tm2 + pvx[i,:]' * pvx[j,:]
       end
    end
  end

  return (tm1-tm2)[2:(length(b)+1), 2:(length(b)+1)]
end

#---------------------------------------------------------------------------------------------#

# Bayesian Estimation 

# MCMC Settings
burnin = 1000
thin   = 20
draws  = 1000
totdraws = draws*thin + burnin

@everywhere np = 3; # Number of parameters, relevant for estimation.

## Auxiliary prior settings, and computations
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

# Obtain the hessian for each consumer's fractional likelihood
# Optimization for simple logit demand - use as starting point for other computations.
@everywhere b = zeros(Float64, np)
@everywhere llx(b::Vector) = ll(b, dt)
@everywhere llg!(b::Vector, storage::Vector) = (storage[:] = llg(b,dt))
@everywhere llh!(b::Vector, storage::Matrix) = (storage[:] = llh(b,dt))
@everywhere kopt = optimize(llx, llg!, zeros(Float64, np), method = :bfgs, grtol=1e-10, ftol = 1e-32)

Hess = SharedArray(Float64, np, np, nh);
@sync @parallel for h in 1:nh
   Hess[:,:,h] = H(h, kopt.minimum)
end

#Initialize storage of MCMC draws
bhatd = zeros(Float64, draws, np);
sigd = zeros(Float64, np, np, draws);

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

finalize(workers)
