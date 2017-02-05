# Keurig Machine Adoption Probabilities
# Xiliang Lin
# Jan 2017

#Set Directory
cd("$(homedir())/Keurig");

# Computation Settings
controls = true; # Add controls such as seasonality
cons_av = false; # Constant adoption value

# Load Packages
using Distributions, FastGaussQuadrature, Calculus, Gadfly

# Which Counterfactuals
#Choice: 1. Original Household; 2. Homogeneous Household; 3. None variety seeking.
ctype = 1
#Which: Util type 1. Only GMCR; 2.
mtype = 1

# Mean estimates of coefficients
burnin = 15000
coef_mcmc = readdlm("Data/Bayes-MCMC/Adoption-Coef-With-MU-Wide.csv");
theta = [0.1, -0.1]
kappa = mean(coef_mcmc[(burnin+1):end, 3:(end-1)], 1)[1,:]

# Parameter settings
β  = 0.995;
if ctype == 1
  # δ' = α0 + α1⋅δ + ϵ, ϵ~N(0,σ0^2)
  α0 = 0.0019029;
  α1 = 0.9308280;
  σ0 = 0.03114;
elseif ctype == 2
  α0 = 0.0018778;
  α1 = 0.8963512;
  σ0 = 0.03548;
elseif ctype == 3
  α0 = 0.0018778;
  α1 = 0.8963512;
  σ0 = 0.03548;
else
  throw(DomainError())
end

# Reference Price: p_ref' = ω⋅price + (1-ω)⋅p_ref
ω = 0.3243178
# Price: price' = ρ0 + ρ1⋅price + ɛ, ɛ~N(0,σ1^2)
ρ0 = 6.3093504
ρ1 = 0.9541681
σ1 = 7.230754

# Load all function
include("$(homedir())/Keurig/Scripts/Counterfactuals/functions.jl")

# Chebyshev Approximation Setup
nodes = 20; # Degree of Chebyshev Zeros (nodes)
order = 5; # Degree of Chebyshev Polynomials
(a, b) = (0., 20.); # a need to be greater/equal to 1 for the function to be well behaved.
(cnode, cweight)=gausschebyshev(nodes); # Interpolation Nodes
tcnode = (cnode+1) * (b-a)/2 + a; # Transformed nodes

W1 = zeros(Float64, nodes);
Tmat = Float64[T(i,j) for i = 1:(order+1), j in cnode]; # Chebyshev Polynomial Matrix

# Compute Value function
if cons_av
  W1coef = zeros(Float64, order+1)
else
  tol = 1e-8
  err = 1;
  nx = 0;
  while (err > tol)
      nx = nx+1;
      W1n = W1V(W1)
      err = sum(abs(W1n-W1))
      #println("Error is $(err), and interation is $(nx)")
      W1 = W1n
  end
  W1coef = getcoef(W1)
end

# Readin data to estimate adoption probability on
if ctype == 1
  hh_panel = readdlm("Data/Counterfactual/Type-MU-Panel.csv", ',', skipstart=1);
elseif ctype == 2
  hh_panel = readdlm("Data/Counterfactual/Homo-MU-Panel.csv", ',', skipstart=1);
elseif ctype == 3
  hh_panel = readdlm("Data/Counterfactual/NoVariety-MU-Panel.csv", ',', skipstart=1);
end

# NA list
None_NA_list=Array(Int64, 0)
for i in 1:size(hh_panel, 1)
  if (typeof(hh_panel[i,9]))<:Real
    push!(None_NA_list, i)
  end
end
hh_panel = hh_panel[None_NA_list, :];
hh_key = convert(Array{Int64}, hh_panel[:,[1,4]])
XMat = convert(Array{Float64}, hh_panel[:, [5, 6, (7+mtype)]])
ZMat = hh_panel[:, 11:16]
ZMat = sparse(convert(Array{Float64}, ZMat))
pbar_n2 =  ω * XMat[:,1] + (1-ω) * XMat[:,2];
SMat = transpose(hcat(ρ0 + ρ1 * pbar_n2, pbar_n2, α0 + α1 * XMat[:,3]))
(nobs, n_x) = size(XMat)
n_z= size(ZMat)[2]
pbar_n2 = 0;
gc();
W1vec=W1fun(nobs)

# Compute the adoption Probabilities
npar = n_x - 1  + n_z;

pd = Uniform(minimum(XMat[:,1]), maximum(XMat[:,1]))
pbard = Uniform(minimum(XMat[:,2]), maximum(XMat[:,2]))
mud = Uniform(minimum(XMat[:,3]), maximum(XMat[:,3]))

sH = diagm([5^2, 5^2, 0.03^2]);
N_0 = 5000
xtild = zeros(Float64, n_x, N_0);
stild = zeros(Float64, n_x, N_0);
wtild = zeros(Float64, N_0);
for i in 1:N_0
  xtild[:,i] = [rand(pd), rand(pbard), rand(mud)]
  pbar_n2 = ω*xtild[1, i] + (1-ω)*xtild[2, i]
  stild[:,i] = [ρ0 + ρ1*pbar_n2, pbar_n2, α0 + α1*xtild[3, i]]
end

xpdfm = zeros(Float64, N_0, N_0);
spdfm = zeros(Float64, N_0, N_0);
for i in 1:N_0
  x_dist = MvNormal(xtild[:,i], sH)
  xpdfm[i, :] = 10000*pdf(x_dist, xtild)
  s_dist = MvNormal(stild[:,i], sH)
  spdfm[i, :] = 10000*pdf(s_dist, xtild)
end

# Value if adopting
EW1x = zeros(Float64, N_0);
for i in 1:N_0
  EW1x[i] = theta[1] + theta[2] * xtild[1, i] + coefun(W1coef, xtild[3, i])
end

tol = 1e-8
err = 1;
nx = 0;
while (err > tol)
    nx = nx+1;
    approxW = (xpdfm' * wtild)./(sum(xpdfm, 1)[1,:])
    wnext = (spdfm' * wtild)./(sum(spdfm, 1)[1,:])
    wtild1 = 30*log(exp(β * wnext/30) + exp(EW1x/30))
    err = sum(abs(wtild1-wtild))
    wtild[:] = wtild1
    println("Error is $(err), and interation is $(nx)")
end

# Compute the adoption probability
probv = zeros(Float64, 100)
zb = ZMat*kappa
for i in 1:100
  s_dist = MvNormal(SMat[:,i], sH)
  sden = 10000*pdf(s_dist, xtild)
  wnext = ((sden' * wtild)/sum(sden))[1]
  EW1= theta[1] + theta[2] * XMat[i, 1] + coefun(W1coef, XMat[i, 3])
  Em = mean(vcat(β * wnext/30, EW1/30 + zb[i]))
  probv[i] = exp(β * wnext/30 - Em)/(exp(β * wnext/30 - Em) + exp(EW1/30 + zb[i]-Em))
end
fname = string("Data/Counterfactual/Prob_type_",ctype,"_mu_",mtype,".csv")
#writedlm(fname, hcat(hh_key, probv), ',')
