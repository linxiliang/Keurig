# Keurig Machine Adoption Estimation
# Xiliang Lin
# Jan 2017

# Setting for parallel computation
test_run = false;
remote = false;
if remote
  addprocs(10, restrict=false)
  machines = [("bushgcn02", 10), ("bushgcn03", 10), ("bushgcn04", 10), ("bushgcn05", 10), ("bushgcn06", 10)]
  # machines = [("bushgcn33", 28)]
  # machines = [("bushgcn11", 12), ("bushgcn12", 12)]
  addprocs(machines; tunnel=true)
else
  addprocs(32; restrict=false)
end
np = workers()
remotecall_fetch(rand, 2, 20) # Test worker

function broad_mpi(expr::Expr)
  for p in np
      @spawnat p eval(expr)
  end
end
broad_mpi(:(np = $np));

#Set Directory
@everywhere cd("$(homedir())/Keurig");

# Computation Settings
acadjust = false; # Whether to adjust for crowded product space
@everywhere controls = true; # Add controls such as seasonality
@everywhere cons_av = false; # Constant adoption value

# Load Packages
using Distributions, Optim, FastGaussQuadrature, Calculus, ChebyshevApprox, StatsBase, BlackBoxOptim;
broad_mpi(:(using Distributions, Optim, FastGaussQuadrature, Calculus, ChebyshevApprox, StatsBase, BlackBoxOptim;))

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
# Price scaling constant
@everywhere pscale = 100.0
@everywhere ω = 0.2703183
# Price: price' = ρ0 + ρ1⋅p_ref + ɛ, ɛ~N(0,σ1^2)
@everywhere ρ1 = 0.9411549
@everywhere ρ0 = 0.2764826 + (ρ1-1.0)*log(pscale)
#@everywhere ρ2 = 0.00025473
@everywhere σ1 = 0.0536

# Load all function
@everywhere include("$(homedir())/Keurig/Scripts/Two-Step-Demand/Machine-Adoption/functions.jl")
@everywhere include("$(homedir())/Keurig/Scripts/Two-Step-Demand/Machine-Adoption/functions-ML-LN-Bayes.jl")

# Read estimation data
if acadjust
  hh_panel = readdlm("Data/Machine-Adoption/HW-MU-Panel.csv", ',', skipstart=1); # Household Machine Adoption Panel
else
  hh_panel = readdlm("Data/Machine-Adoption/HW-MU-Panel-NoAdjust.csv", ',', skipstart=1); # Household Machine Adoption Panel
end
hh_panel[:, 4] = log.(hh_panel[:, 4])

# NA list
None_NA_list=Array{Int64}(0)
for i in 1:size(hh_panel, 1)
  if (typeof(hh_panel[i,9]))<:Real
    push!(None_NA_list, i)
  end
end
hh_panel = hh_panel[None_NA_list, :];

if test_run
  indx = sort(sample(1:size(hh_panel)[1], 80000, replace = false))
  hh_panel = hh_panel[indx, :];
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
@everywhere XMat[:, 1:2] = XMat[:, 1:2]./100
@everywhere ZMat = hh_panel[:, vcat(10:15, 4)] # With a linear time trend
# @everywhere ZMat = hh_panel[:, 10:15]
@everywhere ZMat = sparse(convert(Array{Float64}, ZMat))
@everywhere pbar_n2 =  ω * XMat[:,1] + (1-ω) * XMat[:,2];
# @everywhere SMat = transpose(hcat(ρ0 + ρ1 * log.(pbar_n2) + ρ2 * log.(XMat[:,3].+1), pbar_n2, α0 + α1 * log.(XMat[:,3].+1)))
@everywhere SMat = transpose(hcat(ρ0 + ρ1 * log.(pbar_n2), log.(pbar_n2), α0 + α1 * log.(XMat[:,3].+1)))
@everywhere (nobs, n_x) = size(XMat)
@everywhere n_z= size(ZMat)[2]
@everywhere pbar_n2 = 0;
@everywhere gc();

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
      err = maximum(abs(W1n-W1))
      println("Error is $(err), and interation is $(nx)")
      W1 = W1n
  end
end
broad_mpi(:(delta_cheby_weights = $delta_cheby_weights))
@everywhere wappx(x::Real) = chebyshev_evaluate(delta_cheby_weights,[x],delta_order,delta_range)
@everywhere W1Vec = [wappx(XMat[i,3]) for i in 1:nobs]

# Chebyshev Approximation Settings of W function
@everywhere n1 = 15;
@everywhere n2 = 15;
@everywhere n3 = 15;

range_1 = [maximum(XMat[:,1])*1.2, minimum(XMat[:,1])*0.8]
range_2 = [maximum(XMat[:,2])*1.2, minimum(XMat[:,2])*0.8]
range_3 = [maximum(XMat[:,3])*1.2, minimum(XMat[:,3])*0.8]
broad_mpi(:(range_1 = $range_1))
broad_mpi(:(range_2 = $range_2))
broad_mpi(:(range_3 = $range_3))
@everywhere range = [range_1 range_2 range_3]

@everywhere nodes_1 = chebyshev_nodes(n1,range_1)
@everywhere nodes_2 = chebyshev_nodes(n2,range_2)
@everywhere nodes_3 = chebyshev_nodes(n3,range_3)

@everywhere order_tensor = [5, 5, 5]

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
broad_mpi(:(EW1x = $EW1x))

# Bellman Iteration
tol = 1e-8;
@everywhere sigma = [σ1, σ0];
@everywhere w1_b = Array(Float64, nobs);
@everywhere w0_b = Array(Float64, nobs);
wgrid = zeros(Float64, n1,n2,n3)
wgrid_new = SharedArray(Float64, n1, n2, n3);
ExpectedW = zeros(Float64, n1,n2,n3)
ll_w_grad = zeros(Float64, length(np), n_z)
# Θ_0 = [-1000.73282815577369, 1.94909, 7.960764, 0.0001, 0.35536066706454367, -0.10823587535855583, 0.2486100147839871, 0.2248080375561089, 0.6206315818689631]
# Θ_0 = [-762.079,1.774,10.7504, 0.13671037841663608026, 0.79119170457287690823, 0.35703443779432908478, 0.42949855017590554684, 0.01820777905828507501, -0.13282396850696606694]
@everywhere κ = zeros(Float64, n_z)
Θ_0 = [-9.53665, 1.17825, 0.0705927]
Θ_a = [-9.53665, 1.17825, 0.0705927]
380.464528 seconds (28.85 M allocations: 1.885 GiB, 0.12% gc time)
julia> @time optimize(ll_int!, κ, NelderMead())
120.464183 seconds (8.29 M allocations: 662.520 MiB, 0.45% gc time)
# Here!!!!

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
    wgrid = (thtild[3,:].^2).*log(exp(β*wnext./(thtild[3,:].^2)).+exp(EW1./(thtild[3,:].^2)))
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
