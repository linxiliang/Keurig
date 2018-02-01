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
# theta = [-762.079,1.774,10.7504]
# kappa = [0.13671037841663608026, 0.79119170457287690823, 0.35703443779432908478, 0.42949855017590554684,
# 0.01820777905828507501, -0.13282396850696606694]
theta = [-683.456,0.384917,8.12834]
kappa = [0.161922,0.907353,0.412566,0.496336,0.0222774,-0.130797]

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
    α0 = 0.1175695;
    α1 = 0.91436821;
    σ0 = 0.4414554;
  elseif ctype == 2
    α0 = 0.1075383;
    α1 = 0.91522945;
    σ0 = 0.6143332;
  elseif ctype == 3
    α0 = 0.07449771;
    α1 = 0.8959553;
    σ0 = 0.5967287;
  else
    throw(DomainError())
  end
end

# Reference Price: p_ref' = ω⋅price + (1-ω)⋅p_ref
ω = 0.286745
# Price: price' = ρ0 + ρ1⋅price + ɛ, ɛ~N(0,σ1^2)
ρ0 = 7.2280977
ρ1 = 0.9498492
σ1 = 6.62

# Load all function
include("$(homedir())/Keurig/Scripts/Counterfactuals/functions-ML.jl")

# Chebyshev Approximation Setup
delta_n = 20; # Degree of Chebyshev Zeros (nodes)
delta_order = [5]; # Degree of Chebyshev Polynomials
delta_range = [60., -10.] # Range of option value
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

# Readin data to estimate adoption probability
if acadjust
  hh_panel = readdlm("Data/Counterfactual/HW-MU-Decomp.csv", ',', skipstart=1);
else
  hh_panel = readdlm("Data/Counterfactual/HW-MU-Decomp-NoAdjust.csv", ',', skipstart=1);
end
if ctype == 1
  mindx = 8
elseif ctype == 2
  mindx = 9
elseif ctype == 3
  mindx = 10
end

hh_key = convert(Array{Int64}, hh_panel[:,[1,4]])
XMat = convert(Array{Float64}, hh_panel[:, [5, 6, mindx]])
ZMat = hh_panel[:, 11:16]
ZMat = sparse(convert(Array{Float64}, ZMat))
pbar_n2 =  ω * XMat[:,1] + (1-ω) * XMat[:,2];
SMat = transpose(hcat(ρ0 + ρ1 * pbar_n2, pbar_n2, α0 + α1 * XMat[:,3]))
(nobs, n_x) = size(XMat)
n_z= size(ZMat)[2]
W1Vec = [wappx(XMat[i,3]) for i in 1:nobs]
pbar_n2 = 0;
gc();

# Chebyshev Approximation of W function
n1 = 10;
n2 = 10;
n3 = 10;

range_1 = [211.243, 69.99]
range_2 = [211.243, 71.99]
range_3 = [50.0399, 0.0]
range = [range_1 range_2 range_3]

nodes_1 = chebyshev_nodes(n1,range_1)
nodes_2 = chebyshev_nodes(n2,range_2)
nodes_3 = chebyshev_nodes(n3,range_3)

wgrid = zeros(Float64, n1,n2,n3)
order_tensor = [3, 3, 3]

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
    mu = [ρ0 + ρ1 * pbar_n2, α0 + α1 * nodes_3[k]];
    EW_v0 = β * hermiteint2d(w_tensor, mu, sigma, pbar_n2)/(theta[3]^2);
    EW_v1 = (theta[1] - (theta[2]^2) * nodes_1[i] + EW1x[k])/(theta[3]^2);
    wgrid_new[i, j, k] = theta[3]^2 * log(exp(EW_v0) + exp(EW_v1))
  end
  err = sum(abs(wgrid_new-wgrid))
  println("Error is $(err), and interation is $(nx)")
  wgrid[:,:,:] = wgrid_new;
end
w_tensor = chebyshev_weights(wgrid, nodes_1, nodes_2, nodes_3, order_tensor, range);

# Compute the adoption probability
w1_b = Array(Float64, nobs);
w0_b = Array(Float64, nobs);
zb = ZMat*kappa;
w1_b[:] = (theta[1] - (theta[2]^2) * XMat[:,1] +  W1Vec)/(theta[3]^2) + zb;
# EWmax = maximum(vcat(EW1x, wtild))
for i in 1:nobs
  w0_b[i] = β*hermiteint2d(w_tensor, [SMat[1,i], SMat[3,i]], sigma, SMat[2,i]);
end
probv = exp(w1_b)./(exp(w0_b/(theta[3]^2))+exp(w1_b));

if acadjust
  fname = string("Data/Counterfactual/decomp_type_",ctype,".csv")
else
  fname = string("Data/Counterfactual/noadjust_decomp_type_",ctype,".csv")
end

writedlm(fname, hcat(hh_key, probv), ',')
