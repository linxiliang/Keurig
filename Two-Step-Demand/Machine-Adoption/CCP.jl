# Keurig Machine Adoption Using IJC Algorithm

# Treat price index and adoption value as continuous variables.

#Settings
#Set Local Directory

@everywhere cd("$(homedir())/Keurig");

#Load Packages
@everywhere using Distributions;
using Optim;
using FastGaussQuadrature;
using Calculus;

# Solve the Dynamic Programming Problem Once
# Solve for the Value of holding a Keurig Machine versus not holding
# As I assume the consumer would have the machine forever after purchase, I then first need to solve the Bellman equation for holding the machine. W(1,ν)=ν+∫W(1,ν′)dF(ν′), where ν′∼N(μ(ν),σ2(ν)).
# Here we assume ν′=α0+α1ν+ϵ where ϵ∼N(0,1)
# Set γ
@everywhere β  = 0.995
α0 = 0.006759535;
α1 = 0.9097726;
σ0 = 0.01694262;

# Grid of ν
@everywhere nodes = 20; # Degree of Chebyshev Zeros (nodes)
@everywhere order = 5; # Degree of Chebyshev Polynomials
@everywhere (a, b) = (0., 6.); # a need to be greater/equal to 1 for the function to be well behaved.
(cnode, cweight)=gausschebyshev(nodes); # Interpolation Nodes
tcnode = (cnode+1) * (b-a)/2 + a; # Transformed nodes

# Initialize W(ν)
W1 = zeros(Float64, nodes);
@everywhere T(i, x) = cos((i-1) .* acos(x));
Tmat = Float64[T(i,j) for i = 1:(order+1), j in cnode]; # Chebyshev Polynomial Matrix 

# Version 2
function capprox(w, x::Real)
    length(w) == nodes ? length(w) : error("function values not the same length as interpolation nodes")
    Wmat = repmat(w, 1, (order+1)); # Step 3 - function value at transformed node
    ccoef = diag(Tmat * Wmat) ./ sum(Tmat.^2,2)[:,1]; #Step 4 Compute coefficients
    if (a<=x<=b)
        return sum(Float64[T(i, 2 * (x-a)/(b-a) - 1) * ccoef[i] for i = 1:(order+1)]);
    elseif (x<a)
        abase = capprox(w, a)
        aslope = (capprox(w, a+0.0001) - abase)/0.0001
        return abase + aslope * (x-a) # Linear Extrapolation
    else
        bbase = capprox(w, b)
        bslope = (bbase - capprox(w, b - 0.0001))/0.0001
        return bbase + bslope * (x-b) # Linear Extrapolation
    end
end

# Version 3
@everywhere function coefun(ccoef, x::Real)
    if (a<=x<=b)
        return sum(Float64[T(i, 2. * (x-a)/(b-a) - 1.) * ccoef[i] for i = 1:(order+1)]);
    elseif (x<a)
        abase = coefun(ccoef, a)
        aslope = (coefun(ccoef, a+0.0001) - abase)/0.0001
        return abase + aslope * (x-a) # Linear Extrapolation
    else
        bbase = ccoef(ccoef, b)
        bslope = (bbase - coefun(ccoef, b - 0.0001))/0.0001
        return bbase + bslope * (x-b) # Linear Extrapolation
    end
end

# Get Coefficient function
function getcoef(w)
    Wmat = repmat(w, 1, (order+1)); # Step 3 - function value at transformed node
    ccoef = diag(Tmat * Wmat) ./ sum(Tmat.^2,2)[:,1]; #Step 4 Compute coefficients
    return ccoef
end

(hermitenodes, hermitewts)=gausshermite(9);
function hermiteint(f::Function, μ::Float64, σ::Float64)
    return sqrt(1./pi) * sum(map(f, sqrt(2.)*σ*hermitenodes + μ) .* hermitewts);
end

# Bellman - Value Function Iteration
function W1V(W)
    wappx(x::Real) = capprox(W, x);
    function wfun(ν::Real)
        return ν + β * hermiteint(wappx, α0+α1*ν, σ0)
    end
    return map(wfun, tcnode)
end

# Solve the Bellman equation for valuations.
tol = 1e-8
err = 1; 
nx = 0;
while (err > tol)
    nx = nx+1;
    W1n = W1V(W1)
    err = sum(abs(W1n-W1))
    # println("Error is $(err), and interation is $(nx)")
    W1 = W1n
end

# Obtain the coefficients for chebyshev
W1coef = getcoef(W1)

# Estimate the Dynamic Program using IJC Algorithm
# Simulate the choices given these variables
true_theta = [3.455148, -2.785514]

# Support of the state variables.
pd = Uniform(80., 200.)
pbard = Uniform(80., 200.)
mud = Uniform(0., 6.)

# Generate the random state
ngrid = 3000
rstate = hcat(rand(pd, ngrid), rand(pbard, ngrid), rand(mud, ngrid))'

# Value Function Approximation
h = 1.
@everywhere function WApprox(s1::Vector, s0::Matrix, h::Float64, wvec::Vector)
    pbar_n2 = 0.3617623*s1[1] + 0.6382377*s1[2]
    pbar_n1 = 8.5120709 + 0.9389002*pbar_n2
    sbar_n = 0.006759535 + 0.9097726*s1[3]
    sdist = MvNormal([pbar_n1, pbar_n2, sbar_n], diagm([3.683^2, h, 0.6308^2]))
    wts = pdf(sdist, s0)
    wts[:] = wts ./ sum(wts)
    return (sum(wts .* wvec))
end


# Bellman Iteration
@everywhere function W0V(θ1::Vector, s1::Vector, w::Float64)
    return 100*log(exp(0.995*w/100) + exp((θ1[1] + θ1[2] * s1[1] + coefun(W1coef, s1[3]))/100))
end

# Estimate the Value Function
wtild0 = zeros(Float64, ngrid)

# Solve the Bellman Equation over these random grids
tol = 1e-8
err = 1; 
nx = 0;
while (err > tol)
    nx = nx+1;    
    wtild1 = Float64[W0V(true_theta, rstate[:,i], WApprox(rstate[:,i], rstate, h, wtild0)) for i in 1:ngrid]
    err = sum(abs(wtild1-wtild0))
    println("Error is $(err), and interation is $(nx)")
    wtild0 = wtild1
end


# Simulate the purchase decision
price = collect(100:0.5:180)
delta = collect(0:0.01:6)
pbar = 140.

pb = zeros(Float64, length(price) * length(delta))
k = 0
for p in price, d in delta
    k+=1
    ex0 = exp(0.995 * WApprox([p, pbar, d], rstate, h, wtild0)/100)
    ex1 = exp((true_theta[1] + true_theta[2] * p + coefun(W1coef, d))/100)
    pb[k] = ex1/(ex0+ex1)
    println("Current i is $k, price is $p, and delta is $d")
end

writedlm("Data/Bayes-MCMC/ccp-grid.csv", thatd) # Write the posterior average coefficients
