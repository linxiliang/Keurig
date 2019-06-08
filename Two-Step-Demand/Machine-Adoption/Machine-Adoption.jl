# Keurig Machine Adoption Estimation
# Xiliang Lin
# May 2019

# Setting for parallel computation
test_run = false;
remote = true;
if remote
  addprocs(32, restrict=false)
  # machines = [("bushgcn02", 10), ("bushgcn03", 10), ("bushgcn04", 10), ("bushgcn05", 10), ("bushgcn06", 10)]
  machines = [("10.252.198.12", 32)]
  addprocs(machines; tunnel=true)
else
  # addprocs(32; restrict=false)
  addprocs(2; restrict=false)
end
np = workers()
println("number of works is $(np)!")
remotecall_fetch(rand, 2, 20) # Test worker

function broad_mpi(expr::Expr)
  for p in np
      @spawnat p eval(expr)
  end
end
broad_mpi(:(np = $np));

# Set Seed
srand(123)

#Set Directory
@everywhere cd("$(homedir())/Keurig");
# @everywhere cd("/Volumes/SanDisk/Keurig")

# Computation Settings
acadjust = false;
@everywhere controls = true; # Add controls such as seasonality
@everywhere cons_av = false; # Constant adoption value

# Load Packages
using Distributions, Optim, FastGaussQuadrature, Calculus, ChebyshevApprox, StatsBase
broad_mpi(:(using Distributions, Optim, FastGaussQuadrature, Calculus, ChebyshevApprox, StatsBase))

# Parameter settings
@everywhere β  = 0.995;
# δ' = α0 + α1⋅δ + ϵ, ϵ~N(0,σ0^2)
if acadjust
  @everywhere α0 = 0.1323974;
  @everywhere α1 = 0.91602396;
  @everywhere σ0 = 0.5124003;
else
  @everywhere α0 = 0.0115345;
  @everywhere α1 = 0.9863566;
  @everywhere σ0 = 0.1086;
end

# Reference Price: p_ref' = ω⋅price + (1-ω)⋅p_ref
@everywhere pscale = 100.0 # Price scaling constant
@everywhere ω = 0.2703183
# Price: price' = ρ0 + ρ1⋅p_ref + ɛ, ɛ~N(0,σ1^2)
@everywhere ρ1 = 0.9411549
@everywhere ρ0 = 0.2764826 + (ρ1-1.0)*log(pscale)
#@everywhere ρ2 = 0.00025473
@everywhere σ1 = 0.0536

# Read estimation data
if acadjust
  hh_panel = readdlm("Data/Machine-Adoption/HW-MU-Panel.csv", ',', skipstart=1); # Household Machine Adoption Panel
else
  hh_panel = readdlm("Data/Machine-Adoption/HW-MU-Panel-NoAdjust.csv", ',', skipstart=1); # Household Machine Adoption Panel
end
hh_panel[:, 4] = log.(hh_panel[:, 4])

if test_run
  hh_panel = hh_panel[1:100000, :];
end

(nr, nc) = size(hh_panel)
chunk_size = ceil(Int, nr/length(np))
@sync begin
    for p in np
        if p != np[end]
            r_expr = :(hh_panel = $(hh_panel[((p-2)*chunk_size+1):((p-1)*chunk_size), :]))
        else
            r_expr = :(hh_panel = $(hh_panel[((p-2)*chunk_size+1):end, :]))
        end
        @spawnat p eval(r_expr)
    end
end

@everywhere purch_vec = convert(Array{Int64}, hh_panel[:, 9])
@everywhere XMat = convert(Array{Float64}, hh_panel[:, [5, 6, 8]])
@everywhere XMat[:, 1:2] = XMat[:, 1:2]./pscale
@everywhere ZMat = hh_panel[:, vcat(10:15, 4)]
@everywhere ZMat = sparse(convert(Array{Float64}, ZMat))
@everywhere pbar_n2 =  ω * XMat[:,1] + (1-ω) * XMat[:,2];
# @everywhere SMat = transpose(hcat(ρ0 + ρ1 * pbar_n2, pbar_n2, α0 + α1 * XMat[:,3]))
@everywhere SMat = transpose(hcat(ρ0 + ρ1 * log.(pbar_n2), log.(pbar_n2), α0 + α1 * log.(XMat[:,3].+1)))
@everywhere (nobs, n_x) = size(XMat)
@everywhere n_z= size(ZMat)[2]
@everywhere pbar_n2 = 0;
@everywhere gc();

# Load all function
# @everywhere include("$(homedir())/Keurig/Scripts/Two-Step-Demand/Machine-Adoption/functions.jl")
@everywhere include("Scripts/Two-Step-Demand/Machine-Adoption/functions.jl")

# Chebyshev Approximation Setup
@everywhere delta_n = 15; # Degree of Chebyshev Zeros (nodes)
@everywhere delta_order = [5]; # Degree of Chebyshev Polynomials
delta_range = [maximum(XMat[:,3])*1.2, minimum(XMat[:,3])*0.8] # Range of option value
broad_mpi(:(delta_range = $delta_range))
@everywhere delta_nodes = chebyshev_nodes(delta_n, delta_range)
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
      err = maximum(abs.(W1n-W1))
      println("Error is $(err), and interation is $(nx)")
      W1 = W1n
  end
end
broad_mpi(:(delta_cheby_weights = $delta_cheby_weights))
@everywhere wappx(x::Real) = chebyshev_evaluate(delta_cheby_weights,[x],delta_order,delta_range)
@everywhere W1Vec = [wappx(XMat[i,3]) for i in 1:nobs]

# MCMC Draws
burnin = 0;
thin   = 1;
draws  = 20000;
totdraws = draws*thin + burnin;
npar = n_x + n_z;

# Only use IJC to approximate theta, co-variates serve as auxilary variables.
bhat = zeros(Float64, n_x)
sigb = eye(n_x)*100

# Propose a starting value and the random walk steps（why these settings?)
@everywhere theta0 = [-10.2, -2.0, 10.0]
kappa0 = zeros(Float64, n_z)
sigs = diagm([3.1, 0.3, 0.035])
walkdistr = MvNormal(zeros(n_x), sigs);

# Obtain the range of the state variables
pd = Uniform(minimum(XMat[:,1]), maximum(XMat[:,1]))
pbard = Uniform(minimum(XMat[:,2]), maximum(XMat[:,2]))
mud = Uniform(minimum(XMat[:,3]), maximum(XMat[:,3]))

# IJC Approximation Settings
@everywhere sH = diagm([3*σ1^2, 3*σ0^2, 3*σ0^2]);
@eval @everywhere H = 1/2 * $sigs # Kernal bandwidth
@everywhere N_0 = 1000

# Obtain the initial approximation grid for theta and given states
thtild = theta0 .+ 1*rand(walkdistr, N_0); # theta grid
xtild = zeros(Float64, n_x, N_0); # x - grid
stild = zeros(Float64, n_x, N_0); # future log of the state grid
wtild = ones(Float64, N_0); # gdoption value grid
for i in 1:N_0
  xtild[:,i] = [rand(pd), rand(pbard), rand(mud)]
  pbar_n2 = ω * xtild[1, i] + (1-ω)*xtild[2, i]
  stild[:,i] = [ρ0 + ρ1*log(pbar_n2), log(pbar_n2), α0 + α1 * log(xtild[3, i]+1)]
end
txtild = copy(xtild); # Transformed x-grid for approximation purpose
txtild[1, :] = log.(txtild[1, :]);
txtild[2, :] = log.(txtild[2, :]);
txtild[3, :] = log.(txtild[3, :] + 1); # log(delta + 1)

# Obtain the kernal value
tpdfm = zeros(Float64, N_0, N_0);
spdfm = zeros(Float64, N_0, N_0);
for i in 1:N_0
  t_dist = MvNormal(thtild[:,i], H)
  tpdfm[i, :] = pdf(t_dist, thtild)
  s_dist = MvNormal(stild[:,i], sH) # It's stild since it represents the mean state variables
  spdfm[i, :] = pdf(s_dist, txtild)
end

# The distribution of next period appropriate log transformed states are
# functions of last periods approxiated states' log transformation.

# Value if adopting
EW1x = zeros(Float64, N_0);
for i in 1:N_0
  EW1x[i] = wappx(xtild[3, i])
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
    wgrid = abs.(thtild[3,:]) .* log.(exp.(β * (wnext ./ abs.(thtild[3,:]))).+exp.(EW1 ./ abs.(thtild[3,:])))

    err = sum(abs(wgrid-wtild))
    wtild[:] = wgrid
    println("Error is $(err), and interation is $(nx)")
end

# Broadcast the estimated values to all workers
broad_mpi(:(thtild = $thtild));
broad_mpi(:(xtild = $xtild));
broad_mpi(:(txtild = $txtild));
broad_mpi(:(stild = $stild));
broad_mpi(:(wtild = $wtild));

# param distributions and pdfs
tdist0 = MvNormal(theta0, H);
tpdf =  pdf(tdist0, thtild);
broad_mpi(:(tpdf=$tpdf));
tpdf1 = copy(tpdf); # Why is this needed?
broad_mpi(:(tpdf1=$tpdf1));

# state distributions
sdist = Array(Distributions.MvNormal{Float64,PDMats.PDMat{Float64,Array{Float64,2}},Array{Float64,1}},0)
for i in 1:N_0
    push!(sdist, MvNormal(txtild[:,i], sH))
end
@sync broad_mpi(:(sdist = $sdist))

# Compute state pdfs
@sync broad_mpi(:(spdf = spzeros(nobs, N_0))) # Sparse matrix
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
@sync broad_mpi(:(w1_old = (theta0[1] + theta0[2] * XMat[:,1] + W1Vec)/abs(theta0[3])));
@sync broad_mpi(:(delta_old = Array{Float64}(nobs)))

# Initialize storage in other processes
@sync broad_mpi(:(wts = Array{Float64}(nobs)))
@sync broad_mpi(:(ww = Array{Float64}(nobs)))
@sync broad_mpi(:(w1_new = Array{Float64}(nobs)))
@sync broad_mpi(:(delta_new = Array{Float64}(nobs)))
# Push the transformed state variable to state distributions
snew_dist = MvNormal(randn(n_x), sH)
broad_mpi(:(snew_dist = $snew_dist))

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
    s_i = [ρ0 + ρ1 * log(pbar_n2), log(pbar_n2), α0 + α1 * log(x_i[3]+1)]
    tx_i = copy(x_i)
    tx_i[1] = log(tx_i[1]);
    tx_i[2] = log(tx_i[2]);
    tx_i[3] = log(tx_i[3] + 1); # log(delta + 1)

    # Push the transformed state variable to state distributions
    snew_dist = MvNormal(tx_i, sH)
    broad_mpi(:(snew_dist = $snew_dist))

    # Push s_i to the new values
    @everywhere push!(sdist, snew_dist)
    @everywhere shift!(sdist)

    # Approximate the expected value using s_i
    w_i = WApprox(theta1, thtild, H, s_i, txtild, sH, wtild)

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

    # Compute likelihood for the old and new theta
    opt_old = optimize(logit_ll_old_agg, logit_grad_old_agg!, kappa0, BFGS())
    opt_new = optimize(logit_ll_new_agg, logit_grad_new_agg!, kappa0, BFGS())

    # M-H step
    alpha = min(1, postd(-opt_new.minimum, -opt_old.minimum, theta1, theta0, bhat, sigb))
    if (rand()<=alpha)
      theta0 = theta1
      @sync broad_mpi(:(theta0 = $theta1))
      ll0 = -opt_old.minimum
      if controls
        kappa0 = opt_new.minimizer
      end
      @sync broad_mpi(:(supdate!())) # Update only if accepted to get new estimates
    else
      ll0 = -opt_old.minimum
      kappa0 = opt_old.minimizer
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
    println("Current parameters")
    elapsed_t = time_ns() - start_time;
    if controls
      println(vcat(theta0, kappa0))
      println(vcat(theta1, opt_new.minimizer))
    else
      println(theta0)
      println(theta1)
    end
    println("Likelihood comparison")
    println("Old likelihood $(- opt_old.minimum) v.s. new likelihood $(- opt_new.minimum)")
    @printf("%10.6f seconds has passed\n", elapsed_t/1e9)
    println("Finished drawing $(d) out of $(totdraws)")
end

# writedlm("Data/Bayes-MCMC/Adoption-Coef-MU-F40000.csv", hcat(thatd, lld))
