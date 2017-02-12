# MCMC Summaries of Adoption Parameters
# Xiliang Lin
# Jan 2017

#Set Directory
cd("$(homedir())/Keurig");
srand(12345678)

# Load Packages
using Distributions, FastGaussQuadrature, Calculus, PyPlot
using QuantEcon:meshgrid

# Read the mcmcdraws
theta = [-8.25372,-0.0168313,0.132642]
n_x = length(theta)

# Parameter settings
β  = 0.995;
# δ' = α0 + α1⋅δ + ϵ, ϵ~N(0,σ0^2)
α0 = 0.006460989;
α1 = 0.944186643;
σ0 = 0.05756;
# Reference Price: p_ref' = ω⋅price + (1-ω)⋅p_ref
ω = 0.3243178
# Price: price' = ρ0 + ρ1⋅price + ɛ, ɛ~N(0,σ1^2)
ρ0 = 6.3093504
ρ1 = 0.9541681
σ1 = 7.230754

############
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

############
# Draws
pd = Uniform(80, 200);
pbard = Uniform(130, 150);
mud = Uniform(0, 2.5);

sH = diagm([σ1^2, 5^2, σ0^2]);
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
  xpdfm[i, :] = pdf(x_dist, xtild)
  s_dist = MvNormal(stild[:,i], sH)
  spdfm[i, :] = pdf(s_dist, xtild)
end

# Value if adopting
EW1x = zeros(Float64, N_0);
for i in 1:N_0
  EW1x[i] = theta[1] + theta[2] * xtild[1, i] + theta[3] * coefun(W1coef, xtild[3, i])
end

tol = 1e-8;
err = 1;
nx = 0;
while (err > tol)
    nx = nx+1;
    EWmax = maximum(vcat(EW1x, wtild))
    wnext = (spdfm * wtild)./(sum(spdfm, 2)[:,1])
    wgrid = log(exp(β * wnext - EWmax) + exp(EW1x - EWmax)) + EWmax
    err = sum(abs(wgrid-wtild))
    wtild[:] = wgrid
    println("Error is $(err), and interation is $(nx)")
end

# X grid
ngrid = 200
pgrid = zeros(Float64, ngrid)
mgrid = zeros(Float64, ngrid)
pstep = (180-100)/ngrid
mstep = 2/ngrid
for i in 1:ngrid
  pgrid[i] = 100+pstep*(i-1)
  mgrid[i] = mstep*(i-1)
end

probv = zeros(Float64, ngrid, ngrid)
for i in 1:ngrid
  for j in 1:ngrid
    pbar =  ω * pgrid[i] + (1-ω) * 140.;
    pkc = ρ0 + ρ1 * pbar
    smu = α0 + α1 * mgrid[j]
    s_dist = MvNormal([pkc, pbar, smu], sH)
    sden = pdf(s_dist, xtild)
    wnext = (sum(sden' * wtild)/sum(sden))
    EW1= theta[1] + theta[2] * pgrid[i] + theta[3]*coefun(W1coef, mgrid[j])
    probv[i,j] =  exp(β * wnext)/(exp(EW1) + exp(β*wnext))
  end
end
probv = 1.-probv
writedlm("/Users/xlin0/Desktop/ccp.csv", probv, ",")

fig = figure(figsize=(8,6))
ax = fig[:gca](projection="3d")
ax[:set_zlim](0, 0.05)
δ, Price = meshgrid(mgrid, pgrid)
ax[:plot_surface](δ, Price, probv, rstride=2, cstride=2, cmap=ColorMap("jet"), alpha=0.7, linewidth=0.25)
