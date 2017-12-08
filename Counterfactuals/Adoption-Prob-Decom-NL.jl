# Keurig Machine Adoption Probabilities
# Xiliang Lin
# Jan 2017

#Set Directory
cd("$(homedir())/Keurig");
srand(12345678)

# Load Packages
using Distributions, Optim, FastGaussQuadrature, Calculus, ChebyshevApprox, StatsBase, BlackBoxOptim

# Which Counterfactuals
#Choice: 1. Original, 2. No best match, 3. Only best match
ctype = 1
acadjust = false;

# Mean estimates of coefficients
theta = [-811.565,1.04924,8.85432]
kappa = [0.400385,0.914904,0.603365,0.370014,0.116253,-0.0275953,0.570688]

# Parameter settings
β  = 0.995;
# δ' = α0 + α1⋅δ + ϵ, ϵ~N(0,σ0^2)
if acadjust
  if ctype == 1
    α0 = 0.1323974;
    α1 = 0.91602396;
    σ0 = 0.5124003;
  elseif ctype == 2
    α0 = 0.1307429;
    α1 = 0.90187381;
    σ0 = 0.5124003;
  elseif ctype == 3
    α0 = 0.07686407;
    α1 = 0.90440030;
    σ0 = 0.5124003;
  else
    throw(DomainError())
  end
else
  if ctype == 1
    α0 = 0.02552319;
    α1 = 0.96285965;
    σ0 = 0.1081999;
  elseif ctype == 2
    α0 = 0.03430837;
    α1 = 0.94291733;
    σ0 = 0.1230313;
  elseif ctype == 3
    α0 = 0.02745073;
    α1 = 0.92454671;
    σ0 = 0.1175826;
  else
    throw(DomainError())
  end
end

# Reference Price: p_ref' = ω⋅price + (1-ω)⋅p_ref
ω = 0.2939151
# Price: price' = ρ0 + ρ1⋅price + ɛ, ɛ~N(0,σ1^2)
ρ0 = 0.3427638
ρ1 = 0.93099801
σ1 = 0.04110696


# Load all function
include("$(homedir())/Keurig/Scripts/Counterfactuals/functions-ML-LN.jl")

# Readin data to estimate adoption probability
if acadjust
  hh_panel = readdlm("Data/Counterfactual/HW-MU-Decomp.csv", ',', skipstart=1);
else
  hh_panel = readdlm("Data/Counterfactual/HW-MU-Decomp-NoAdjust.csv", ',', skipstart=1);
end

# NA list
None_NA_list=Array(Int64, 0)
for i in 1:size(hh_panel, 1)
  if (typeof(hh_panel[i,9]))<:Real
    push!(None_NA_list, i)
  end
end
hh_panel = hh_panel[None_NA_list, :];
(nr, nc) = size(hh_panel)

if ctype == 1
  mindx = 8
elseif ctype == 2
  mindx = 9
elseif ctype == 3
  mindx = 10
end

hh_key = convert(Array{Int64}, hh_panel[:,[1,4]])
hh_panel[:, 4] = log(hh_panel[:, 4])
hh_panel[:, mindx] = 1.01 * hh_panel[:, mindx]
XMat = convert(Array{Float64}, hh_panel[:, [5, 6, mindx]])
ZMat = hh_panel[:, vcat(11:16, 4)]
ZMat = sparse(convert(Array{Float64}, ZMat))
pbar_n2 =  ω * XMat[:,1] + (1-ω) * XMat[:,2];
SMat = transpose(hcat(ρ0 + ρ1 * log(pbar_n2), pbar_n2, α0 + α1 * log(XMat[:,3].+1)))
(nobs, n_x) = size(XMat)
n_z= size(ZMat)[2]
pbar_n2 = 0;
gc();

# Chebyshev Approximation Setup
delta_n = 20; # Degree of Chebyshev Zeros (nodes)
delta_order = [5]; # Degree of Chebyshev Polynomials
delta_range = [32.93, -0.] # Range of option value
delta_nodes = chebyshev_nodes(delta_n, delta_range)
W1 = zeros(Float64, delta_n);

# Compute Value function
tol = 1e-8
err = 1;
nx = 0;
delta_cheby_weights = zeros(Float64, delta_order[1]+1)
while (err > tol)
    nx = nx+1;
    delta_cheby_weights[:] = chebyshev_weights(W1, delta_nodes, delta_order, delta_range)
    W1n = W1V(delta_cheby_weights)
    err = sum(abs(W1n-W1))
    println("Error is $(err), and interation is $(nx)")
    W1 = W1n
end
wappx(x::Real) = chebyshev_evaluate(delta_cheby_weights,[x],delta_order,delta_range)
W1Vec = [wappx(XMat[i,3]) for i in 1:nobs]

# Chebyshev Approximation of W function
n1 = 12;
n2 = 12;
n3 = 12;

range_1 = [239.99, 39.99]
range_2 = [215.031, 79.98]
range_3 = [32.93, 0.0]
range = [range_1 range_2 range_3]

nodes_1 = chebyshev_nodes(n1,range_1)
nodes_2 = chebyshev_nodes(n2,range_2)
nodes_3 = chebyshev_nodes(n3,range_3)

wgrid = zeros(Float64, n1,n2,n3)
order_tensor = [5, 5, 5]

mgrid = Tuple{Int, Int, Int64}[]
for i in 1:n1
  for j in 1:n2
    for k in 1:n3
      push!(mgrid, (i,j,k))
    end
  end
end

# Value of adoption
EW1x = zeros(Float64,n3);
for i in 1:n3
  EW1x[i] = wappx(nodes_3[i])
end

# Bellman Iteration
tol = 1e-8;
sigma = [σ1, σ0];
err = 1;
nx = 0;
wgrid_new = Array(Float64, n1, n2, n3);
while (err > tol)
  nx = nx+1;
  w_tensor = chebyshev_weights(wgrid, nodes_1, nodes_2, nodes_3, order_tensor, range);
  for (i,j,k) in mgrid
    pbar_n2 =  ω * nodes_1[i] + (1-ω) * nodes_2[j];
    mu = [ρ0 + ρ1 * log(pbar_n2), α0 + α1 * log(nodes_3[k]+1)];
    EW_v0 = β * hermiteint2d(w_tensor, mu, sigma, pbar_n2)/(theta[3]^2);
    EW_v1 = (theta[1] - (theta[2]^2) * nodes_1[i] + EW1x[k])/(theta[3]^2);
    wgrid_new[i, j, k] = theta[3]^2 * log(exp(EW_v0) + exp(EW_v1))
  end
  err = sum(abs(wgrid_new-wgrid))
  println("Error is $(err), and interation is $(nx)")
  wgrid[:,:,:] = wgrid_new;
end
w_tensor = chebyshev_weights(wgrid, nodes_1, nodes_2, nodes_3, order_tensor, range);

ExpectedW = zeros(Float64, n1,n2,n3)
for (i,j,k) in mgrid
  pbar_n2 =  ω * nodes_1[i] + (1-ω) * nodes_2[j];
  # mu = [ρ0 + ρ1 * log(pbar_n2) + ρ2 * log(nodes_3[k]+1), α0 + α1 * log(nodes_3[k]+1)];
  mu = [ρ0 + ρ1 * log(pbar_n2), α0 + α1 * log(nodes_3[k]+1)];
  ExpectedW[i, j, k] = β * hermiteint2d(w_tensor, mu, sigma, pbar_n2)/(theta[3]^2);
end
w_tensor = chebyshev_weights(ExpectedW, nodes_1, nodes_2, nodes_3, order_tensor, range);

# Compute the adoption probability
zb = ZMat*kappa;
w1_b = (theta[1] - (theta[2]^2) * XMat[:,1] +  W1Vec)/(theta[3]^2) + zb;
w0_b = Array(Float64, nobs);
# EWmax = maximum(vcat(EW1x, wtild))
for h in 1:nobs
  w0_b[h] = chebyshev_evaluate(w_tensor, XMat[h,:], order_tensor, range);
end
probv =  exp(w1_b)./(exp(w0_b)+exp(w1_b));

if acadjust
  fname = string("Data/Counterfactual/decomp_type_",ctype,".csv")
else
  fname = string("Data/Counterfactual/noadjust_decomp_type_",ctype,".csv")
end

writedlm(fname, hcat(hh_key, probv), ',')
