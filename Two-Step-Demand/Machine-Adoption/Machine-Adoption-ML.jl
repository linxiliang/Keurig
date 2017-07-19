# Keurig Machine Adoption Estimation
# Xiliang Lin
# Jan 2017

# Setting for parallel computation
test_run = true;

#Set Directory
cd("$(homedir())/Keurig");

# Computation Settings
controls = true; # Add controls such as seasonality
cons_av = false; # Constant adoption value

# Load Packages
using Distributions, Optim, FastGaussQuadrature, Calculus, ChebyshevApprox

# Parameter settings
β  = 0.995;
# δ' = α0 + α1⋅δ + ϵ, ϵ~N(0,σ0^2)
α0 = 0.1323974;
α1 = 0.91602396;
σ0 = 0.5124003;
# Reference Price: p_ref' = ω⋅price + (1-ω)⋅p_ref
ω = 0.286745
# Price: price' = ρ0 + ρ1⋅price + ɛ, ɛ~N(0,σ1^2)
ρ0 = 7.2280977
ρ1 = 0.9498492
σ1 = 6.62

# Load all function
include("$(homedir())/Keurig/Scripts/Two-Step-Demand/Machine-Adoption/functions-ML.jl")

# Chebyshev Approximation Setup
delta_n = 20; # Degree of Chebyshev Zeros (nodes)
delta_order = [5]; # Degree of Chebyshev Polynomials
delta_range = [60., -10.] # Range of option value
delta_nodes = chebyshev_nodes(delta_n, delta_range)
W1 = zeros(Float64, delta_n);

# Compute Value function
if cons_av
  delta_cheby_weights = zeros(Float64, delta_order+1)
else
  tol = 1e-8
  err = 1;
  nx = 0;
  while (err > tol)
      nx = nx+1;
      delta_cheby_weights = chebyshev_weights(W1, delta_nodes, delta_order, delta_range)
      W1n = W1V(delta_cheby_weights)
      err = sum(abs(W1n-W1))
      println("Error is $(err), and interation is $(nx)")
      W1 = W1n
  end
end
wappx(x::Real) = chebyshev_evaluate(delta_cheby_weights,[x],delta_order,delta_range)

# Read estimation data
hh_panel = readdlm("Data/Machine-Adoption/HW-MU-Panel.csv", ',', skipstart=1); # Household Machine Adoption Panel

# NA list
None_NA_list=Array(Int64, 0)
for i in 1:size(hh_panel, 1)
  if (typeof(hh_panel[i,9]))<:Real
    push!(None_NA_list, i)
  end
end
hh_panel = hh_panel[None_NA_list, :];
if test_run
  hh_panel = hh_panel[1:100000, :];
end
(nr, nc) = size(hh_panel)

purch_vec = convert(Array{Int64}, hh_panel[:, 9])
XMat = convert(Array{Float64}, hh_panel[:, [5, 6, 8]])
ZMat = hh_panel[:, vcat(10:15, 4)]
ZMat = sparse(convert(Array{Float64}, ZMat))
pbar_n2 =  ω * XMat[:,1] + (1-ω) * XMat[:,2];
SMat = transpose(hcat(ρ0 + ρ1 * pbar_n2, pbar_n2, α0 + α1 * XMat[:,3]))
(nobs, n_x) = size(XMat)
n_z= size(ZMat)[2]
W1vec = [wappx(XMat[i,3]) for i in 1:nobs]
pbar_n2 = 0;
gc();

# Compute the states


# MCMC Draws
burnin = 0;
thin   = 1;
draws  = 20000;
totdraws = draws*thin + burnin;
npar = n_x + n_z;

if controls
  bhat = zeros(Float64, npar)
  sigb = eye(npar)*100
else
  bhat = zeros(Float64, n_x)
  sigb = eye(n_x)*100
end

# Propose a starting value
@everywhere theta0 = [-10.2, -2.0, 2.0]
@everywhere kappa0 = zeros(Float64, n_z)
sigs = diagm([3.1, 0.3, 0.035])
walkdistr = MvNormal(zeros(n_x), sigs);
ksigs = diagm([0.378, 0.1558, 0.2232, 0.0102, 0.3646, 0.19914, 1.727257e-06])
kwalkdis = MvNormal(zeros(n_z), ksigs);

pd = Uniform(minimum(XMat[:,1]), maximum(XMat[:,1]))
pbard = Uniform(minimum(XMat[:,2]), maximum(XMat[:,2]))
mud = Uniform(minimum(XMat[:,3]), maximum(XMat[:,3]))

@everywhere sH = diagm([3*σ1^2, 9.^2, 3*σ0^2]);
@eval @everywhere H = 1/2 * $sigs
@everywhere N_0 = 1500
thtild = theta0 .+ 1*rand(walkdistr, N_0);
xtild = zeros(Float64, n_x, N_0);
stild = zeros(Float64, n_x, N_0);
wtild = ones(Float64, N_0);
for i in 1:N_0
  xtild[:,i] = [rand(pd), rand(pbard), rand(mud)]
  pbar_n2 = ω*xtild[1, i] + (1-ω)*xtild[2, i]
  stild[:,i] = [ρ0 + ρ1*pbar_n2, pbar_n2, α0 + α1*xtild[3, i]]
end

tpdfm = zeros(Float64, N_0, N_0);
spdfm = zeros(Float64, N_0, N_0);
for i in 1:N_0
  t_dist = MvNormal(thtild[:,i], H)
  tpdfm[i, :] = pdf(t_dist, thtild)
  s_dist = MvNormal(stild[:,i], sH)
  spdfm[i, :] = pdf(s_dist, xtild)
end

# Value if adopting
EW1x = zeros(Float64,N_0);
for i in 1:N_0
  EW1x[i] = coefun(W1coef, xtild[3, i])
end

tol = 1e-6
err = 1;
nx = 0;
while (err > tol)
    nx = nx+1;
    # Now approximate the next period W
    wnext = ((spdfm.*tpdfm) * wtild)./(sum((spdfm.*tpdfm), 2)[:,1])

    # Obtain the Value for adopting
    EW1 = thtild[1,:] + thtild[2, :] .* xtild[1, :] + EW1x

    # Compute the Bellman
    EWmax = maximum(vcat(EW1x, β*wnext))
    wgrid = exp(thtild[3,:]).*log(exp(β*wnext./exp(thtild[3,:])).+exp(EW1./exp(thtild[3,:])))
    err = sum(abs(wgrid-wtild))
    wtild[:] = wgrid
    println("Error is $(err), and interation is $(nx)")
end

broad_mpi(:(thtild = $thtild));
broad_mpi(:(stild = $stild));
broad_mpi(:(wtild = $wtild));

# param distributions and pdfs
tdist0 = MvNormal(theta0, H);
tpdf =  pdf(tdist0, thtild);
broad_mpi(:(tpdf=$tpdf));
tpdf1 = tpdf;
broad_mpi(:(tpdf1=$tpdf1));

# state distributions
sdist = Array(Distributions.MvNormal{Float64,PDMats.PDMat{Float64,Array{Float64,2}},Array{Float64,1}},0)
for i in 1:N_0
    push!(sdist, MvNormal(stild[:,i], sH))
end
@sync broad_mpi(:(sdist = $sdist))

# Compute state pdfs
@sync broad_mpi(:(spdf = spzeros(nobs, N_0)))
@everywhere function sden!()
    for i in 1:N_0
        spdf[:, i] = pdf(sdist[i], SMat)
        println("Current iteration is $i out of $(N_0)")
    end
end
@sync broad_mpi(:(sden!()))

# Compute the relevante vectors concerning theta0
@sync broad_mpi(:(wts_old  = spdf * tpdf));
@sync broad_mpi(:(ww_old = spdf * (tpdf .* wtild)));
@sync broad_mpi(:(w1_old = (theta0[1] + theta0[2] * XMat[:,1] + W1vec)/exp(theta0[2]) + ZMat*kappa0));

# Initialize storage in other processes
@sync broad_mpi(:(wts = Array(Float64, nobs)))
@sync broad_mpi(:(ww = Array(Float64, nobs)))
@sync broad_mpi(:(w1_new = Array(Float64, nobs)))

# Initialize MCMC storage
if controls
  thatd = zeros(Float64, draws, npar);
else
  thatd = zeros(Float64, draws, n_x);
end
lld = zeros(Float64, draws);
start_time = time_ns();
for d=1:totdraws
    # Proposed theta
    theta1 = theta0 + rand(walkdistr)
    broad_mpi(:(theta1 = $theta1))

    # Propose Kappa
    if controls
      kappa1 = kappa0 + rand(kwalkdis)
      broad_mpi(:(kappa1 = $kappa1))
    end

    if (d==5000)
      @eval @everywhere H = 1/9 * $sigs
    end

    # Compute the pdfs
    tdist1 = MvNormal(theta1, H);
    broad_mpi(:(tdist1 = $tdist1))

    # tnew pdf value
    tnew = pdf(tdist1, theta0);
    broad_mpi(:(tnew=$tnew))

    # Draw a state proposal
    x_i = [rand(pd), rand(pbard), rand(mud)]
    pbar_n2 = ω*x_i[1] + (1-ω)*x_i[2]
    s_i = [ρ0 + ρ1*pbar_n2, pbar_n2, α0 + α1*x_i[3]]

    snew_dist = MvNormal(x_i, sH)
    broad_mpi(:(snew_dist = $snew_dist))

    # Push s_i to the new values
    push!(sdist, snew_dist)
    shift!(sdist)

    # Approximate the Value function using s_i
    w_i = WApprox(theta1, thtild, H, s_i, stild, sH, wtild)

    # Bellman Iteration Once using x_i
    wnew = W0V(theta1, x_i, w_i)
    broad_mpi(:(wnew = $wnew))

    # Update the draws storage (thtild -- theta matrix updated)
    thtild[:, 1:(end-1)] = thtild[:, 2:end]
    thtild[:, end] = theta1
    broad_mpi(:(thtild[:, 1:(end-1)] = thtild[:, 2:end]))
    broad_mpi(:(thtild[:, end] = theta1))

    # Update states
    @sync broad_mpi(:(vupdate!()))

    # Compute likelihood for the new
    llv = ll!(ll_fun, np);
    println(llv)

    if controls
      alpha = min(1, postd(llv[1,2], llv[1,1], theta1, theta0, kappa1, kappa0, bhat, sigb))
    else
      alpha = min(1, postd(llv[1,2], llv[1,1], theta1, theta0, bhat, sigb))
    end

    if (rand()<=alpha)
    　　theta0 = theta1
       @sync broad_mpi(:(theta0 = $theta1))
       ll0 = llv[1,2]
       if controls
         kappa0 = kappa1
         @sync broad_mpi(:(kappa0 = $kappa1))
       end
       @sync broad_mpi(:(supdate!()))
    else
       ll0 = llv[1,1]
    end

    # Store the posterior draws of mean betas and sigmas
    if ((d > burnin) && (d % thin == 0))
        indx = ceil(Int64, (d - burnin)/thin)
        if  controls
          thatd[indx, :] = vcat(theta0, kappa0);
        else
          thatd[indx, :] = theta0
        end
        lld[indx] = ll0;
    end

    # Indicate the progress
    elapsed_t = time_ns() - start_time;
    if controls
      println(vcat(theta0, kappa0))
      println(vcat(theta1, kappa1))
    else
      println(theta0)
      println(theta1)
    end
    @printf("%10.6f seconds has passed\n", elapsed_t/1e9)
    println("Finished drawing $(d) out of $(totdraws)")
end

writedlm("Data/Bayes-MCMC/Adoption-Coef-MU-F40000.csv", hcat(thatd, lld))
