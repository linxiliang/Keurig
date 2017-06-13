# Keurig Machine Adoption Probabilities
# Xiliang Lin
# Jan 2017

#Set Directory
cd("$(homedir())/Keurig");
srand(12345678)

# Trial Run
test_run = false

# Computation Settings
controls = true; # Add controls such as seasonality
cons_av = false; # Constant adoption value

# Load Packages
using Distributions, FastGaussQuadrature, Calculus, Gadfly

# Which Counterfactuals
#Choice: 1. Original Household; 2. Homogeneous Household; 3. None variety seeking.
ctype = 3
#Which: Util type
mtype = 3

# Mean estimates of coefficients
burnin = 15000
coef_mcmc = readdlm("Data/Bayes-MCMC/Adoption-Coef-With-MU-Wide.csv");
theta = [-19.2934, -2.39987, 3.54114]
#kappa = mean(coef_mcmc[(burnin+1):end, 3:(end-1)], 1)[1,:]
kappa = [-0.113607,0.0102209,0.197621,0.0185101,-0.309528,0.0320812,0.00471407]

# Parameter settings
β  = 0.995;
if ctype == 1 && mtype == 1
  # δ' = α0 + α1⋅δ + ϵ, ϵ~N(0,σ0^2)
  α0 = 0.07377136;
  α1 = 0.86713638;
  σ0 = 0.22683156;
elseif ctype == 1 && mtype == 2
  α0 = 0.07970123;
  α1 = 0.87145641;
  σ0 = 0.24702640;
elseif ctype == 1 && mtype == 3
  α0 = 0.07493004;
  α1 = 0.85704155;
  σ0 = 0.21146857;
elseif ctype == 2 && mtype == 1
  α0 = 0.1047945;
  α1 = 0.8978939;
  σ0 = 0.2701943;
elseif ctype == 2 && mtype == 2
  α0 = 0.1078744;
  α1 = 0.9176983;
  σ0 = 0.3404092;
elseif ctype == 2 && mtype == 3
  α0 = 0.1002870;
  α1 = 0.9095506;
  σ0 = 0.2848422;
elseif ctype == 3 && mtype == 1
  α0 = 0.07836079;
  α1 = 0.86833808;
  σ0 = 0.24258531;
elseif ctype == 3 && mtype == 2
  α0 = 0.08485846;
  α1 = 0.87146810;
  σ0 = 0.26178065;
elseif ctype == 3 && mtype == 3
  α0 = 0.08067107;
  α1 = 0.85677889;
  σ0 = 0.22517781;
else
  throw(DomainError())
end

# Reference Price: p_ref' = ω⋅price + (1-ω)⋅p_ref
@everywhere ω = 0.2806031
# Price: price' = ρ0 + ρ1⋅price + ɛ, ɛ~N(0,σ1^2)
@everywhere ρ0 = 14.87545
@everywhere ρ1 = 0.8975656
@everywhere σ1 = 4.928914

# Load all function
include("$(homedir())/Keurig/Scripts/Counterfactuals/functions.jl")

# Chebyshev Approximation Setup
nodes = 20; # Degree of Chebyshev Zeros (nodes)
order = 5; # Degree of Chebyshev Polynomials
(a, b) = (0., 40.); # a need to be greater/equal to 1 for the function to be well behaved.
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
if test_run
  hh_panel = hh_panel[1:100000, :]
end
hh_key = convert(Array{Int64}, hh_panel[:,[1,4]])
XMat = convert(Array{Float64}, hh_panel[:, [5, 6, (7+mtype)]])
ZMat = hh_panel[:, vcat(11:16, 4)]
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

sH = diagm([9*σ1^2, 10.^2, 9*σ0^2]);
N_0 = 3000
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
  xpdfm[i, :] = pdf(x_dist, xtild)
  s_dist = MvNormal(stild[:,i], sH)
  spdfm[i, :] = pdf(s_dist, xtild)
end

# Value if adopting
EW1x = zeros(Float64, N_0);
for i in 1:N_0
  EW1x[i] = theta[1] + theta[2] * xtild[1, i] + coefun(W1coef, xtild[3, i])
end

tol = 1e-8;
err = 1;
nx = 0;
while (err > tol)
    nx = nx+1;
    EWmax = maximum(vcat(EW1x, wtild))
    wnext = (spdfm * wtild)./(sum(spdfm, 2)[:,1])
    wgrid = exp(theta[3]).*log(exp(β*wnext./exp(theta[3])) .+ exp(EW1x./exp(theta[3])))
    err = sum(abs(wgrid-wtild))
    wtild[:] = wgrid
    println("Error is $(err), and interation is $(nx)")
end

# Compute the adoption probability
probv = zeros(Float64, nobs)
zb = ZMat*kappa
# EWmax = maximum(vcat(EW1x, wtild))
for i in 1:nobs
  s_dist = MvNormal(SMat[:,i], sH)
  sden = pdf(s_dist, xtild)
  wnext = (sum(sden' * wtild)/sum(sden))
  EW1= theta[1] + theta[2] * XMat[i, 1] + coefun(W1coef, XMat[i, 3])
  probv[i] =  exp(β * wnext/exp(theta[3]))/(exp(EW1/exp(theta[3]) + zb[i]) + exp(β*wnext/exp(theta[3])))
end
fname = string("Data/Counterfactual/Prob_type_",ctype,"_mu_",mtype,".csv")
writedlm(fname, hcat(hh_key, probv), ',')
