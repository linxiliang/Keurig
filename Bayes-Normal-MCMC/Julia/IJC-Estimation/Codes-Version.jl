




#Keurig Machine Adoption Using IJC Algorithm

#Settings
#Set Local Directory

@everywhere cd("$(homedir())/Keurig");

#Load Packages
@everywhere using Distributions;
@everywhere using Optim;
@everywhere using FastGaussQuadrature;
@everywhere using DataFrames;
@everywhere using Gadfly;

# Solve the Dynamic Programming Problem Once
# Solve for the Value of holding a Keurig Machine versus not holding
# As I assume the consumer would have the machine forever after purchase, I then first need to solve the Bellman equation for holding the machine. W(1,ν)=ν+∫W(1,ν′)dF(ν′), where ν′∼N(μ(ν),σ2(ν)).
# Here we assume ν′=α0+α1ν+ϵ where ϵ∼N(0,1)
# Set γ
@everywhere β  = 0.995
@everywhere α0 = 0.0958842;
@everywhere α1 = 0.9501078;
@everywhere σ0 = 0.7828;

# Grid of ν
@everywhere nodes = 20; # Degree of Chebyshev Zeros (nodes)
@everywhere order = 5; # Degree of Chebyshev Polynomials
@everywhere (a, b) = (0., 60.); # a need to be greater/equal to 1 for the function to be well behaved.
@everywhere (cnode, cweight)=gausschebyshev(nodes); # Interpolation Nodes
@everywhere tcnode = (cnode+1) * (b-a)/2 + a; # Transformed nodes

# Initialize W(ν)
@everywhere W1 = Array(Float64, nodes);
@everywhere T(i, x) = cos((i-1) .* acos(x));
@everywhere Tmat = Float64[T(i,j) for i = 1:(order+1), j in cnode]; # Chebyshev Polynomial Matrix 

# Version 1
@everywhere function capprox(w, nodes::Int64, order::Int64, a::Real, b::Real, x::Real)
    length(w) == nodes ? length(w) : error("function values not the same length as interpolation nodes")
    (cnode, cweight)=gausschebyshev(nodes) # Step 1 - Chebyshev zeros
    tcnode = (cnode+1) * (b-a)/2 + a; # Step 2 - adjust nodes
    Wmat = repmat(w, 1, (order+1)); # Step 3 - function value at transformed node
    T(i, x) = cos((i-1) .* acos(x)) 
    Tmat = Float64[T(i,j) for i = 1:(order+1), j in cnode];# Chebyshev Polynomial Matrix 
    ccoef = diag(Tmat * Wmat) ./ sum(Tmat.^2,2)[:,1]; #Step 4 Compute coefficients
    if (a<=x<=b)
        return sum(Float64[T(i, 2 * (x-a)/(b-a) - 1) * ccoef[i] for i = 1:(order+1)]);
    elseif (x<a)
        abase = capprox(w, nodes, order, a, b, a)
        aslope = (capprox(w, nodes, order, a, b, a+0.0001) - abase)/0.0001
        return abase + aslope * (x-a) # Linear Extrapolation
    else
        bbase = capprox(w, nodes, order, a, b, b)
        bslope = (bbase - capprox(w, nodes, order, a, b, b - 0.0001))/0.0001
        return bbase + bslope * (x-b) # Linear Extrapolation
    end
end

# Version 2
@everywhere function capprox(w, x::Real)
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
        return sum(Float64[T(i, 2 * (x-a)/(b-a) - 1) * ccoef[i] for i = 1:(order+1)]);
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
@everywhere function getcoef(w)
    Wmat = repmat(w, 1, (order+1)); # Step 3 - function value at transformed node
    ccoef = diag(Tmat * Wmat) ./ sum(Tmat.^2,2)[:,1]; #Step 4 Compute coefficients
    return ccoef
end

#Gauss Hermite Integration Function
# Version 1
@everywhere function hermiteint(f::Function, nodes::Int64, μ::Real, σ::Real)
    (x, wx)=gausshermite(nodes);
    return sqrt(1./pi) * sum(map(f, sqrt(2.)*σ*x + μ) .* wx);
end

# Version 2
@everywhere (hermitenodes, hermitewts)=gausshermite(9);
@everywhere function hermiteint(f::Function, μ::Float64, σ::Float64)
    return sqrt(1./pi) * sum(map(f, sqrt(2.)*σ*hermitenodes + μ) .* hermitewts);
end

# Bellman - Value Function Iteration
@everywhere function W1V(W)
    wappx(x::Real) = capprox(W, x);
    function wfun(ν::Real)
        return ν + β * hermiteint(wappx, α0+α1*ν, σ0)
    end
    return map(wfun, tcnode)
end

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

@everywhere w1appx(x) = capprox(W1, nodes, order, a, b, x)

# Estimate the Dynamic Program using IJC Algorithm
# Load Relevant Data

# Household Machine Adoption Panel
@everywhere hh_adoption = readtable("Data/Machine-Adoption/HH-HW-NewYork-Trips.csv");
@everywhere purch_vec = Array(hh_adoption[:purchased])
@everywhere XSeries = map(Int64, Array(hh_adoption[:series]))
@everywhere XPrice = map(Float64, Array(hh_adoption[:price]))
@everywhere Xt = map(Int64, Array(hh_adoption[:t]))
@everywhere Xmu = map(Float64, Array(hh_adoption[:mu_val]))
@everywhere Xware = map(Int64, Array(hh_adoption[:warehouse]))

# Household Trip Positions
@everywhere hh_pos = map(Int64, readdlm("Data/Machine-Adoption/HH-HW-NewYork-Pos.csv", ','));
@everywhere (ntrips,)= size(hh_pos);

# Price State Panel
@everywhere price_states = readtable("Data/Machine-Adoption/HW-Prices-State.csv");
@everywhere price_states_wh = price_states[price_states[:warehouse].==1, :];
@everywhere price_states_rg = price_states[price_states[:warehouse].==0, :];

# Transform Price States to the Format that can be used
@everywhere price_wh = Dict{Int64, Any}()
@everywhere for i in unique(price_states_wh[:tstate])
    p_temp = price_states_wh[price_states_wh[:tstate].==i,:]
    s_list = unique(p_temp[:series])
    if in(1, s_list)
        p1_list = Array(p_temp[p_temp[:series].==1, :price_int])
        p1_prob = Array(p_temp[p_temp[:series].==1, :prob])
    else 
        p1_list = [9999.]
        p1_prob = [1.]
    end
    if in(2, s_list)
        p2_list = Array(p_temp[p_temp[:series].==2, :price_int])
        p2_prob = Array(p_temp[p_temp[:series].==2, :prob])
    else 
        p2_list = [9999.]
        p2_prob = [1.]
    end
    if in(3, s_list)
        p3_list = Array(p_temp[p_temp[:series].==3, :price_int])
        p3_prob = Array(p_temp[p_temp[:series].==3, :prob])
    else 
        p3_list = [9999.]
        p3_prob = [1.]
    end
    if in(4, s_list)
        p4_list = Array(p_temp[p_temp[:series].==4, :price_int])
        p4_prob = Array(p_temp[p_temp[:series].==4, :prob])
    else 
        p4_list = [9999.]
        p4_prob = [1.]
    end
    if in(5, s_list)
        p5_list = Array(p_temp[p_temp[:series].==5, :price_int])
        p5_prob = Array(p_temp[p_temp[:series].==5, :prob])
    else 
        p5_list = [9999.]
        p5_prob = [1.]
    end
    nprice = length(p1_list) * length(p2_list) * length(p3_list) * length(p4_list) * length(p5_list)
    availvec = zeros(Float64, nprice, 5);
    for s in 1:5
        if in(s, s_list)
            availvec[:,s] = 1.
        end
    end
    pricevec = zeros(Float64, nprice, 5);
    pricep  = zeros(Float64, nprice);
    j = 0
    for (i1, p1) in enumerate(p1_list), (i2, p2) in enumerate(p2_list), (i3, p3) in enumerate(p3_list), (i4, p4) in enumerate(p4_list), (i5, p5) in enumerate(p5_list)
        j += 1;
        pricevec[j,:] = [p1, p2, p3, p4, p5]
        pricep[j] = p1_prob[i1] * p2_prob[i2] * p3_prob[i3] * p4_prob[i4] * p5_prob[i5]
    end    
    pricevec = pricevec[pricep.>=1e-04, :]
    availvec = availvec[pricep.>=1e-04, :]
    pricep = pricep[pricep.>=1e-04]
    pricep = pricep./sum(pricep)
    price_wh[i] = Dict(:price => pricevec, :avail => availvec, :prob => pricep)
end

@everywhere  price_rg = Dict{Int64, Any}()
@everywhere  for i in unique(price_states_rg[:tstate])
    p_temp = price_states_rg[price_states_rg[:tstate].==i,:]
    s_list = unique(p_temp[:series])
    if in(1, s_list)
        p1_list = Array(p_temp[p_temp[:series].==1, :price_int])
        p1_prob = Array(p_temp[p_temp[:series].==1, :prob])
    else 
        p1_list = [9999.]
        p1_prob = [1.]
    end
    if in(2, s_list)
        p2_list = Array(p_temp[p_temp[:series].==2, :price_int])
        p2_prob = Array(p_temp[p_temp[:series].==2, :prob])
    else 
        p2_list = [9999.]
        p2_prob = [1.]
    end
    if in(3, s_list)
        p3_list = Array(p_temp[p_temp[:series].==3, :price_int])
        p3_prob = Array(p_temp[p_temp[:series].==3, :prob])
    else 
        p3_list = [9999.]
        p3_prob = [1.]
    end
    if in(4, s_list)
        p4_list = Array(p_temp[p_temp[:series].==4, :price_int])
        p4_prob = Array(p_temp[p_temp[:series].==4, :prob])
    else 
        p4_list = [9999.]
        p4_prob = [1.]
    end
    if in(5, s_list)
        p5_list = Array(p_temp[p_temp[:series].==5, :price_int])
        p5_prob = Array(p_temp[p_temp[:series].==5, :prob])
    else 
        p5_list = [9999.]
        p5_prob = [1.]
    end
    nprice = length(p1_list) * length(p2_list) * length(p3_list) * length(p4_list) * length(p5_list)
    availvec = zeros(Float64, nprice, 5);
    for s in 1:5
        if in(s, s_list)
            availvec[:,s] = 1.
        end
    end
    pricevec = zeros(Float64, nprice, 5);
    pricep  = zeros(Float64, nprice);
    j = 0
    for (i1, p1) in enumerate(p1_list), (i2, p2) in enumerate(p2_list), (i3, p3) in enumerate(p3_list), (i4, p4) in enumerate(p4_list), (i5, p5) in enumerate(p5_list)
        j += 1;
        pricevec[j,:] = [p1, p2, p3, p4, p5]
        pricep[j] = p1_prob[i1] * p2_prob[i2] * p3_prob[i3] * p4_prob[i4] * p5_prob[i5]
    end
    pricevec = pricevec[pricep.>=1e-04, :]
    availvec = availvec[pricep.>=1e-04, :]
    pricep = pricep[pricep.>=1e-04]
    price_rg[i] = Dict(:price => pricevec, :avail => availvec, :prob => pricep)
end

@everywhere nt = length(unique(price_states[:tstate]))
@everywhere W0_wh = Array(Float64, nt, nodes);
@everywhere W0_rg = Array(Float64, nt, nodes);

# Functions for Approximating the W Function

# Kernal Density Function
@everywhere function kden(θ1::Vector, θ0::Matrix, h::Real)
    cdist = MvNormal(θ1, eye(length(θ1)))
    denv = pdf(cdist, broadcast(-, θ0, θ1)/h)
    return denv ./ sum(denv)
end

# W0 approximation
@everywhere (nr, nc) = (nt, nodes)
@everywhere N = 1000 # Number of lags to store and use for value function approximation.
@everywhere function WApprox(theta::Vector, thetav::Matrix, wmat::Array{Float64,3})
    wts = kden(theta, thetav, 0.1)'
    wt = reshape(repmat(wts, nt*nodes), nodes, nt, N)
    return (sum(wt .* wmat, 3)[:,:,1])'
end

# Bellman Operation on the Value Function once
@everywhere function W0V!(W0::Array{Float64, 2}, wh::Int64)
    for i in 1:(nt-1)
        Wn = W0[(i+1),:]
        EW0Fun(x::Real) = capprox(vec(Wn), x::Real)
        EW0 = exp(β * Float64[hermiteint(EW0Fun, α0+α1*ν, σ0) for ν in tcnode])
        if wh == 1
            nprice = length(price_wh[i][:prob])
            util_mat = sum(exp(broadcast(+, ϑ', βp * price_wh[i][:price])/100) .* price_wh[i][:avail], 2)
            util_mat = broadcast(*, util_mat, repmat(W1', nprice))
            Wnew = sum(broadcast(*, log(broadcast(+, EW0', util_mat)), price_wh[i][:prob]), 1)
        else
            nprice = length(price_rg[i][:prob])
            util_mat = sum(exp(broadcast(+, ϑ', βp * price_rg[i][:price])/100) .* price_rg[i][:avail], 2)
            util_mat = broadcast(*, util_mat, repmat(W1', nprice))
            Wnew = sum(broadcast(*, log(broadcast(+, EW0', util_mat)), price_rg[i][:prob]), 1)
        end
        W0[i,:] = Wnew
    end
    W0[nt,:] = W0[(nt-1),:]
    return 0;
end

# For values of tstate and purchased - function of delta
@everywhere function vfun(series::Int64, price::Float64, ts::Int64, mu::Float64, wh::Int64, W0_wh::Matrix, W0_rg::Matrix, ϑ::Vector, βp::Float64)
    if wh==1
        Wn = vec(W0_wh[ts,:])
    else
        Wn = vec(W0_rg[ts,:])
    end
    coef = getcoef(Wn)
    if series==0
        EW0F(x::Real) = coefun(coef, x::Real)
        val = exp(β*hermiteint(EW0F, α0+α1*mu, σ0))
    else
        val = exp((ϑ[series] + βp*price + w1appx(mu))/100)
    end
    return(val)
end

# Create a New column which stores the choice specific values
@everywhere (nobs, nvars) = size(hh_adoption)
val = SharedArray(Float64, nobs);
function vfun!()
    @sync @parallel for i in 1:nobs
        val[i]= vfun(XSeries[i], XPrice[i], Xt[i], Xmu[i], Xware[i], W0_wh, W0_rg, ϑ, βp)
    end
end

# Likelihood Function
cp = Array(Float64, ntrips)
function llfun!()
    for trip in 1:ntrips
        s_i = hh_pos[trip,2]
        e_i = hh_pos[trip,3]
        cp[trip] = sum(log(val[s_i:e_i]/sum(val[s_i:e_i])) .* purch_vec[s_i:e_i])
    end
    return sum(cp)
end

# Settings for Estimation
N = 100 # Number of lags to store and use for value function approximation.

# Function for Posterior Proportions
function postd(l1::Real, l0::Real, theta1::Vector, theta0::Vector, bhat::Vector, sigb::Matrix)
    return exp(l1 + log(pdf(MvNormal(bhat, sigb), theta1)) - l0 - log(pdf(MvNormal(bhat, sigb), theta0)))
end

# MCMC Settings
burnin = 0;
thin   = 1;
draws  = 10000;
totdraws = draws*thin + burnin;

# Priors
bhat = zeros(Float64, 6)
sigb = eye(6)*100

theta0 = zeros(6)
sigs = eye(6)/100
#sigs[6:6] = 1/300
walkdistr = MvNormal(zeros(6), sigs);

# Initialize storage for proposal parameters and corresponding w
thtild = zeros(Float64, 6, N);
wtild_wh = zeros(Float64, nodes, nt, N);
wtild_rg = zeros(Float64, nodes, nt, N);

# Initialize storage of MCMC draws
thatd = zeros(Float64, draws, 6);

## MCMC Draws
for d=1:totdraws
    # Proposed theta
    theta1 = theta0 + vec(rand(walkdistr, 1))
        
    # Define the relevant parameters
    ϑ = theta1[1:5];
    βp = theta1[6];
    
    # Approximate W function
    W0_wh[:] =  WApprox(theta1, thtild, wtild_wh); # Approximate the new W0_wh
    W0_rg[:] =  WApprox(theta1, thtild, wtild_rg); # Approximate the new W0_rg
    
    # Bellman Iteration
    W0V!(W0_wh, 1);
    W0V!(W0_rg, 0);
    
    # Compute choice specific value
    vfun!(); # Compute the choice specific value for each observation
    l1 = llfun!();
    
    # Update the draws storage
    wtild_wh[:, :, 2:end] = wtild_wh[:, :, 1:(end-1)]
    wtild_wh[:, :, 1] = Array(W0_wh')
    wtild_rg[:, :, 2:end] = wtild_rg[:, :, 1:(end-1)]
    wtild_rg[:, :, 1] = Array(W0_rg')
    thtild[:, 2:end] = thtild[:, 1:(end-1)]
    thtild[:, 1] = theta1
    
   # Define the relevant parameters
    ϑ = theta0[1:5];
    βp = theta0[6];

    # Approximate W function
    W0_wh[:] =  WApprox(theta0, thtild, wtild_wh); # Approximate the new W0_wh
    W0_rg[:] =  WApprox(theta0, thtild, wtild_rg); # Approximate the new W0_rg

    # Bellman Iteration
    W0V!(W0_wh, 1);
    W0V!(W0_rg, 0);

    # Compute choice specific value
    vfun!(); # Compute the choice specific value for each observation
    l0 = llfun!();
    
    println([l0, l1])
    
    alpha = min(1, postd(l1, l0, theta1, theta0, bhat, sigb))
    if rand() <= alpha
        theta0 = theta1
        l0 = l1
    end
                                            
    # Store the posterior draws of mean betas and sigmas
    if ((d > burnin) && (d % thin == 0))
        indx = ceil(Int64, (d - burnin)/thin) 
        thatd[indx, :] = theta0;
    end
                                            
    # Indicate the progress
    println(theta0)
    println("Finished drawing $(d) out of $(totdraws)")
end

writedlm("Data/Bayes-MCMC/Adoption-Coef.csv", thatd) # Write the posterior average coefficients
