# Keurig Machine Adoption Estimation
# Xiliang Lin
# May 2019

# Setting for parallel computation
test_run = false;
remote = false;
if remote
  addprocs(32, restrict=false)
  # machines = [("bushgcn02", 10), ("bushgcn03", 10), ("bushgcn04", 10), ("bushgcn05", 10), ("bushgcn06", 10)]
  machines = [("10.252.198.12", 32)]
  addprocs(machines; tunnel=true)
else
  # addprocs(32; restrict=false)
  addprocs(4; restrict=false)
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
@everywhere include("$(homedir())/Keurig/Scripts/Two-Step-Demand/Machine-Adoption/functions.jl")
# @everywhere include("Scripts/Two-Step-Demand/Machine-Adoption/functions.jl")

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
bhat = [0.0, -1.0, 60]
sigb = eye(n_x)*1000

# Propose a starting value and the random walk steps（why these settings?)
# theta0 = [-10.2, -2.0, 10.0]
# theta0 = [-8.04455, 1.5316, 81.6396]
theta0 = [-7.84943, -2.0931745, 74.6788]
# kappa0 = zeros(Float64, n_z)
kappa0 = [0.461295, 0.974945, 0.64596, 0.294536, 0.0421079, -0.110592, -0.137983]
# sigs = diagm([0.1, 0.1, 0.1])
sigs = 1/3 * [[0.829575  0.255035  1.08045];
              [0.255035  0.815286  0.139934];
              [1.08045   0.139934  3.18221]]
walkdistr = MvNormal(zeros(n_x), sigs);

# Obtain the range of the state variables
pd = Uniform(minimum(XMat[:,1]), maximum(XMat[:,1]))
pbard = Uniform(minimum(XMat[:,2]), maximum(XMat[:,2]))
mud = Uniform(minimum(XMat[:,3]), maximum(XMat[:,3]))

# IJC Approximation Settings
# @everywhere sH = diagm([3.0*σ1^2, 3.0*σ1^2, 3.0*σ0^2]);
@everywhere sH = diagm([σ1^2, 1/100 * σ1^2, σ0^2]);
@eval @everywhere H = 1/100 * $sigs # Kernal bandwidth
# @eval @everywhere H = 1/2 * $sigs # Kernal bandwidth
@everywhere N_0 = 5000

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
# The distribution of next period appropriate log transformed states are
# functions of last periods approxiated states' log transformation.
txtild = copy(xtild); # Transformed x-grid for approximation purpose
txtild[1, :] = log.(txtild[1, :]);
txtild[2, :] = log.(txtild[2, :]);
txtild[3, :] = log.(txtild[3, :] + 1); # log(delta + 1)

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
    wnext = WApprox(thtild, thtild, H, stild, txtild, sH, wtild)

    # Obtain the Value for adopting
    EW1 = thtild[1,:] + thtild[2, :] .* xtild[1, :] + EW1x

    # Compute the Bellman
    wgrid = abs.(thtild[3,:]) .* log.(exp.(β * (wnext ./ abs.(thtild[3,:]))).+exp.(EW1 ./ abs.(thtild[3,:])))

    err = sum(abs(wgrid-wtild))
    wtild[:] = wgrid
    println("Error is $(err), and interation is $(nx)")
end

# Broadcast the estimated values to all workers
broad_mpi(:(thtild = $thtild))
broad_mpi(:(txtild = $txtild))
broad_mpi(:(wtild = $wtild))

# Initialize or compute various wts and ww values
# Storage of old states
@sync broad_mpi(:(wts_old = zeros(nobs)))
@sync broad_mpi(:(ww_old = zeros(nobs)))
@sync broad_mpi(:(WApproxAll!($theta0, wts_old, ww_old))) # Compute the values
@sync broad_mpi(:(w1_old = ($theta0[1] + ($theta0[2]) * XMat[:,1] + W1Vec)/abs($(theta0[3]))))
@sync broad_mpi(:(delta_old = zeros(nobs)))

# Storage of new states
@sync broad_mpi(:(wts_new = zeros(nobs)))
@sync broad_mpi(:(ww_new = zeros(nobs)))
@sync broad_mpi(:(w1_new = zeros(nobs)))
@sync broad_mpi(:(delta_new = zeros(nobs)))

# Initialize MCMC storage
if controls
  thatd = zeros(Float64, draws, npar);
else
  thatd = zeros(Float64, draws, n_x);
end
lld = zeros(Float64, draws);
start_time = time_ns();

# Run the MCMC chain
for d=1:totdraws
    # if (d==5000)
    #    @eval @everywhere H = 1/9 * $sigs
    # end

    # Proposed theta
    theta1 = theta0 + rand(walkdistr)

    # Draw a state proposal
    x_i = [rand(pd), rand(pbard), rand(mud)]
    pbar_n2 = ω*x_i[1] + (1-ω)*x_i[2]
    tx_i = copy(x_i)
    tx_i[1] = log(tx_i[1]);
    tx_i[2] = log(tx_i[2]);
    tx_i[3] = log(tx_i[3] + 1); # log(delta + 1)
    s_i = [ρ0 + ρ1 * log(pbar_n2), log(pbar_n2), α0 + α1 * log(x_i[3]+1)]

    # Approximate the expected value using s_i
    ew_i = WApprox(theta1, thtild, H, s_i, txtild, sH, wtild)

    # Bellman Iteration Once using x_i
    wnew = W0V(theta1, x_i, ew_i)

    # Update W0 states
    broad_mpi(:(WoldStatesUpdate!($theta1, $tx_i, $wnew, $theta0)))

    # Update IJC approximation states
    @everywhere ApproxStateUpdate!($theta1, $tx_i, $wnew)

    # Update deltas
    broad_mpi(:(UpdateStateDeltas!($theta1, $tx_i, $wnew, $theta0)))

    # Compute likelihood for the old and new theta
    opt_old = optimize(logit_ll_old_agg, logit_grad_old_agg!, kappa0, BFGS())
    opt_new = optimize(logit_ll_new_agg, logit_grad_new_agg!, kappa0, BFGS())

    # M-H step
    alpha = min(1, postd(-opt_new.minimum, -opt_old.minimum, theta1, theta0, bhat, sigb))
    if (rand()<=alpha)
      theta0 = theta1
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

writedlm("Data/Bayes-MCMC/Adoption-Coef-IJC-F20000.csv", hcat(thatd, lld))


# Get the variance of each parameters
x1 = [thatd[(i-1)*50 + 1, 1] for i = 101:400]
x2 = [thatd[(i-1)*50 + 1, 2] for i = 101:400]
x3 = [thatd[(i-1)*50 + 1, 3] for i = 101:400]
