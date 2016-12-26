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
# Load Relevant Data

# Household Machine Adoption Panel
@everywhere hh_adoption = readdlm("Data/Machine-Adoption/HH-HW-Panel.csv", ',', skipstart=1)
@everywhere purch_vec = hh_adoption[:, 9]
@everywhere XMat = hh_adoption[:, [5, 6, 8]]
@everywhere ZMat = hh_adoption[:, [4; 12:14]]
AVec = Float64[coefun(W1coef, XMat[i,3]) for i in 1:length(purch_vec)]

# Functions for Approximating the W Function
# Kernal Density Function
# W0 approximation
# θ1 is the parameter vector
# θ0 is the history of stored parameters
# H is bandwidth matrix
# wmat is the stored value functions
@everywhere function WApprox(θ1::Vector, θ0::Matrix, H::Matrix, s1::Vector, s0::Matrix, h::Float64, wvec::Vector)
    cdist = MvNormal(θ1, H)
    wts0 = pdf(cdist, θ0)
    # pbar_n2 = 0.41564*s1[1] + 0.58436*s1[2]
    pbar_n2 = 0.3617623*s1[1] + 0.6382377*s1[2]
    #pbar_n1 = 8.143757 + 0.938283*pbar_n2
    pbar_n1 = 8.5120709 + 0.9389002*pbar_n2
    sbar_n = 0.006759535 + 0.9097726*s1[3]
    # sdist = MvNormal([pbar_n1, pbar_n2, sbar_n], diagm([3.683^2, h, 0.6308^2]))
    sdist = MvNormal([pbar_n1, pbar_n2, sbar_n], diagm([7.348^2, h, 0.01694262^2]))
    wts1 = pdf(sdist, s0)
    wts = wts0 .* wts1
    wts[:] = wts ./ sum(wts)
    return (sum(wts .* wvec))
end

# Maybe vectorizable? 
# cdist is the same for all observations! 
# sdist is different individual by individual, and this is the vectorization challenge!
# But it doesn't change based on theta... So, functions can be modified to be more efficient!
# Computation for wts./sum(wts) is relatively easy...
@everywhere function WApprox(wts0::Vector, wts1::Vector, wvec::Vector)
    wts = wts0 .* wts1
    wts[:] = wts ./ sum(wts)
    return (sum(wts .* wvec))
end

# Bellman Operation on the Value Function once
# Beta is 0.995 here 

# Scaling Version 1
@everywhere function W0V(θ1::Vector, s1::Vector, w::Float64)
    return 100*log(exp(0.995*w/100) + exp((θ1[1] + θ1[2] * s1[1] + coefun(W1coef, s1[3]))/100))
end

# For values of tstate and purchased - function of delta
@everywhere function pfun(θ1::Vector, θ0::Matrix, H::Matrix, KVal::Float64, XVec::Vector, s0::Matrix, h::Float64, wvec::Vector, W1coef::Vector, y::Real)
    ex0 = exp(0.995 * WApprox(θ1, θ0, H, XVec, s0, h, wvec)/100)
    ex1 = exp((θ1[1] + θ1[2] * XVec[1] + coefun(W1coef, XVec[3]))/100 + KVal)
    prb = ex1/(ex0+ex1)
    return(log(prb)*y + log(1-prb)*(1-y))
end

@everywhere function pfun(θ1::Vector, wts0::Vector, wts1::Vector, XVec::Vector, wvec::Vector, AV::Float64, KVal::Float64, y::Real)
    ex0 = exp(0.995 * WApprox(wts0, wts1, wvec)/100)
    ex1 = exp((θ1[1] + θ1[2] * XVec[1] + AV)/100 + KVal)
    prb = ex1/(ex0+ex1)
    return(log(prb)*y + log(1-prb)*(1-y))
end

# Likelihood function
(nobs, nvars) = size(hh_adoption)
function llfun(θ1::Vector, θ0::Matrix, H::Matrix, KVec::Vector, Xmat::Matrix, s0::Matrix, h::Float64, wvec::Vector, W1coef::Vector, y::Vector)
    @sync @parallel (+) for i in 1:nobs
        pfun(θ1, θ0, H, KVec[i], vec(Xmat[i,:]), s0, h, wvec, W1coef, y[i])
    end
end

# updated likelihood function
(nobs, nvars) = size(hh_adoption)
function llfun(θ1::Vector, wts0::Vector, wts1::Matrix, Xmat::Matrix, wvec::Vector, AVec::Vector, KVec::Vector, y::Vector)
    @sync @parallel (+) for i in 1:nobs
        pfun(θ1, wts0, wts1[i,:], vec(Xmat[i,:]), wvec, AVec[i], KVec[i], y[i])
    end
end


# Settings for Estimation
# Function for Posterior Proportions
function postd(l1::Real, l0::Real, theta1::Vector, theta0::Vector, bhat::Vector, sigb::Matrix)
    return exp(l1 + log(pdf(MvNormal(bhat, sigb), theta1)) - l0 - log(pdf(MvNormal(bhat, sigb), theta0)))
end

# MCMC Settings
burnin = 0;
thin   = 1;
draws  = 50000;
totdraws = draws*thin + burnin;
npar = 6;

# Priors
bhat = zeros(Float64, npar)
sigb = eye(npar)*300

#Random Walk Setting
xm = hcat(ones(Float64,length(purch_vec)), XMat[:,1]/100)
sigs = eye(npar)
sigs[1:2,1:2] = inv(xm' * xm) * 9
sigs[3:end,3:end] = inv(ZMat' * ZMat) * 9
walkdistr = MvNormal(zeros(npar), sigs);

# Support of the state variables.
pd = Uniform(minimum(XMat[:,1]), maximum(XMat[:,1]))
pbard = Uniform(minimum(XMat[:,2]), maximum(XMat[:,2]))
mud = Uniform(minimum(XMat[:,3]), maximum(XMat[:,3]))

# Approximation Bandwidth
h = 10.
H = 10*sigs[1:2, 1:2]

# Initialize storage for proposal parameters and corresponding w
N_0 = 1000
thtild = [5.; -5.] .+ 100*rand(walkdistr, N_0)[1:2,:] ;
stild = zeros(Float64, 3, N_0);
wtild = zeros(Float64, N_0);
for i in 1:N_0 stild[:,i] = [rand(pd), rand(pbard), rand(mud)] end
for (k in 1:300)
    for i in 1:N_0
        thtild[:,i] = [5., -5.] + 100*rand(walkdistr)[1:2]
        stild[:,i] = [rand(pd), rand(pbard), rand(mud)]
        wtild[i] = WApprox(thtild[:,i], thtild, H, stild[:,i], stild, h, wtild)
        wtild[i] = W0V(thtild[:,i], stild[:,i], wtild[i])
    end
    #println("Value is supposed to be $(wtild[i])")
end

# Approximation Bandwidth
h = 9.
H = 9*sigs[1:2, 1:2]

# Starting at [0,0]
param0 = [5.; -5.; zeros(Float64, 4)]
theta0 = param0[1:2]
gamma0 = param0[3:end]
KVec0 = ZMat * gamma0

# Initialize storage of MCMC draws
thatd = zeros(Float64, draws, npar);

## MCMC Draws
start_time = now()
for d=1:totdraws
    if d==10000
        h = 1
        H = H/9
    end
   
    # Proposed theta
    param1 = param0 + rand(walkdistr)
    theta1 = param1[1:2]
    gamma1 = param1[3:end]
    KVec1 = ZMat * gamma0

    # Draw a state proposal
    s_i = [rand(pd), rand(pbard), rand(mud)] 
    
    # Approximate the Value function
    w_i = WApprox(theta1, thtild, H, s_i, stild, h, wtild)

    # Bellman Iteration Once
    w_in = W0V(theta1, s_i, w_i)
    
    # Update the draws storage
    # Increase the stored size by 1 for every 10 draw
    if (d % 10 == 0)
        wtild = vcat(wtild, w_in)
        thtild = hcat(thtild, theta1)
        stild = hcat(stild, s_i)
    else
        wtild[1:(end-1)] = wtild[2:end]
        wtild[end] = w_in
        thtild[:, 1:(end-1)] = thtild[:, 2:end]
        thtild[:, end] = theta1
        stild[:, 1:(end-1)] = stild[:, 2:end]
        stild[:, end] = s_i
    end
    
    # Compute likelihood for the new 
    l1 = llfun(theta1, thtild, H, KVec1, XMat, stild, h, wtild, W1coef, purch_vec) # Compute the choice specific value for each observation
    
    # Compute likelihood for the old draw
    l0 = llfun(theta0, thtild, H, KVec0, XMat, stild, h, wtild, W1coef, purch_vec) # Compute the choice specific value for each observation

    println([l0, l1])
    
    alpha = min(1, postd(l1, l0, param1, param0, bhat, sigb))
    if rand() <= alpha
        param0 = param1
        theta0 = theta1
        gamma0 = gamma1
        KVec0 = KVec1
    end
                                            
    # Store the posterior draws of mean betas and sigmas
    if ((d > burnin) && (d % thin == 0))
        indx = ceil(Int64, (d - burnin)/thin) 
        thatd[indx, :] = param0;
    end
                                            
    # Indicate the progress
    println(param0)
    println("Finished drawing $(d) out of $(totdraws), time elapsed $(now()-start_time)")
end

writedlm("Data/Bayes-MCMC/Adoption-Coef.csv", thatd) # Write the posterior average coefficients
