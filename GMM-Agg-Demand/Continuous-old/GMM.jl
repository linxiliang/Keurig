# GMM Estimation of the Aggregate data model
# Xiliang Lin
# Feb, 2018

# Setup parallel computing environment
remote_machine = false;
if remote_machine
  addprocs(20, restrict=false)
  machines = [("bushgcn02", 20), ("bushgcn04", 20), ("bushgcn05", 20), ("bushgcn06", 20)]
  # machines = [("bushgcn33", 28)]
  # machines = [("bushgcn11", 12), ("bushgcn12", 12)]
  addprocs(machines; tunnel=true)
else
  addprocs(8)
end
np = workers();
remotecall_fetch(rand, 2, 20) # Test worker

function broad_mpi(expr::Expr)
  for p in np
      @spawnat p eval(expr)
  end
end
broad_mpi(:(np = $np));

# Set Working directory
# cd("$(homedir())/Keurig")
# @everywhere cd("$(homedir())/Keurig")
cd("/Volumes/SanDisk/Keurig")
@everywhere cd("/Volumes/SanDisk/Keurig");

# Load necessary library
using FastGaussQuadrature
broad_mpi(:(using FastGaussQuadrature))
@everywhere const consumption_rate = 3;
@everywhere (hermite_nodes, hermite_wts) = gausshermite(21);
include("Initialization.jl")
broad_mpi(:(include("Scripts/GMM-Agg-Demand/Initialization.jl")))

# Simulate Data
include("Simulation.jl")

# Given the settings and functions compute the GMM Objective
# market(Z::Array{Float64, 2}, X::Array{Float64, 2}, N::Int64, M::Int64, A::Int64,
# sales::Array{Float64, 1})

function row_major_reshape(X::AbstractArray, dim1, dim2)
  return permutedims(reshape(X, (dim2, dim1)), [2, 1])
end

# Chol2inv function
function cholinv(X)
  try
    U = chol(X)
    UInv = inv(U)
    return U * U'
  catch
    throw(DomainError("The matrix need to be both Positive Definite and Hermitian"))
  end
end

# Outer Constructor to construct types
function IV_WMatfun(listofmarkets, theta = Float64[])
  _ , nIV = size(listofmarkets[1].market.X)
  _ , nX = size(listofmarkets[1].market.X)
  IV = Float64[]
  X = Float64[]
  for m in listofmarkets
    append!(IV, vec(m.market.X'))
    append!(X, vec(m.market.X'))
  end
  IV = row_major_reshape(IV, Integer(length(IV)/nIV), nIV)
  X = row_major_reshape(X, Integer(length(X)/nX), nX)
  IV_temp1 = hcat(IV[: , end].^2, IV[: , end].^3)
  IV_temp2 = IV[: , end] .* IV[: , end-1]
  IV = hcat(IV, IV_temp1, IV_temp2)
  inv_wmat = inv(IV'*IV)
  PreX = inv(X' * X) * X'
  return Dict("X" => X, "IV" => IV, "inv_wmat" => inv_wmat, "PreX" => PreX)
end

function GMMObj(sig_root, IV_WMat, listofmarkets)
  sig = sig_root^2
  N_market = length(listofmarkets)
  sig = ones(N_market) * sig
  xi_d = pmap(xifun, sig, listofmarkets)
  xi = Float64[]
  for i in 1:N_market
    append!(xi, xi_d[i])
  end
  beta = IV_WMat["PreX"] * xi
  resid = xi - IV_WMat["X"] * beta
  return sum(resid.^2)
end

function main(listofmarkets)
  IV_WMat = IV_WMatfun(listofmarkets)
  # Optimize theta
  obj = GMMObj(0.9, IV_WMat, listofmarkets)
  sighat = 2
  # find
  N_market = length(listofmarkets)
  sig = ones(N_market) * sighat
  xi_d = pmap(rxifun, sig, listofmarkets)
  xi = Float64[]
  for i in 1:N_market
    append!(xi, xi_d[i])
  end
  betahat = IV_WMat["PreX"] * xi
  return Dict("beta" => betahat, "sig" => sighat)
end

results = main(market_list)

# Finalize the parallel environment
finalize(workers())
