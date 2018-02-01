# Keurig Machine Adoption Estimation - Maximum Likelihood
# Xiliang Lin
# July 2017

# Setting for parallel computation
test_run = false;
remote = false;
if remote
  addprocs(20, restrict=false)
  machines = [("bushgcn02", 20), ("bushgcn04", 20), ("bushgcn05", 20), ("bushgcn06", 20)]
  # machines = [("bushgcn33", 28)]
  # machines = [("bushgcn11", 12), ("bushgcn12", 12)]
  addprocs(machines; tunnel=true)
else
  addprocs(20; restrict=false)
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
acadjust = false;
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
  @everywhere α0 = 0.104699;
  @everywhere α1 = 0.91714777;
  @everywhere σ0 = 0.4272539;
end

# Reference Price: p_ref' = ω⋅price + (1-ω)⋅p_ref
@everywhere ω = 0.2816747
# Price: price' = ρ0 + ρ1⋅price + ɛ, ɛ~N(0,σ1^2)
@everywhere ρ0 = 7.1280045
@everywhere ρ1 = 0.9504942
@everywhere σ1 = 6.593

# Load all function
@everywhere include("$(homedir())/Keurig/Scripts/Two-Step-Demand/Machine-Adoption/functions-ML.jl")

# Read estimation data
if acadjust
  hh_panel = readdlm("Data/Machine-Adoption/HW-MU-Panel.csv", ',', skipstart=1); # Household Machine Adoption Panel
else
  hh_panel = readdlm("Data/Machine-Adoption/HW-MU-Panel-NoAdjust.csv", ',', skipstart=1); # Household Machine Adoption Panel
end
hh_panel[:, 4] = log(hh_panel[:, 4])

# NA list
None_NA_list=Array(Int64, 0)
for i in 1:size(hh_panel, 1)
  if (typeof(hh_panel[i,9]))<:Real
    push!(None_NA_list, i)
  end
end

hh_panel = hh_panel[None_NA_list, :];
if test_run
  indx = sort(sample(1:3757694, 80000, replace = false))
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
@everywhere ZMat = hh_panel[:, vcat(10:15, 4)] # With a linear time trend
# @everywhere ZMat = hh_panel[:, 10:15]
@everywhere ZMat = sparse(convert(Array{Float64}, ZMat))
@everywhere pbar_n2 =  ω * XMat[:,1] + (1-ω) * XMat[:,2];
@everywhere SMat = transpose(hcat(ρ0 + ρ1 * pbar_n2, pbar_n2, α0 + α1 * XMat[:,3]))
@everywhere (nobs, n_x) = size(XMat)
@everywhere n_z= size(ZMat)[2]
@everywhere pbar_n2 = 0;
@everywhere gc();

# Chebyshev Approximation Setup
@everywhere delta_n = 20; # Degree of Chebyshev Zeros (nodes)
@everywhere delta_order = [5]; # Degree of Chebyshev Polynomials
delta_range = [maximum(XMat[:,3]), minimum(XMat[:,3])] # Range of option value
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
      err = sum(abs(W1n-W1))
      println("Error is $(err), and interation is $(nx)")
      W1 = W1n
  end
end
broad_mpi(:(delta_cheby_weights = $delta_cheby_weights))
@everywhere wappx(x::Real) = chebyshev_evaluate(delta_cheby_weights,[x],delta_order,delta_range)
@everywhere W1Vec = [wappx(XMat[i,3]) for i in 1:nobs]

# Chebyshev Approximation of W function
@everywhere n1 = 12;
@everywhere n2 = 12;
@everywhere n3 = 12;

range_1 = [maximum(XMat[:,1]), minimum(XMat[:,1])]
range_2 = [maximum(XMat[:,2]), minimum(XMat[:,2])]
range_3 = [maximum(XMat[:,3]), minimum(XMat[:,3])]
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
ExpectedW = zeros(Float64, n1,n2,n3)
ll_w_grad = zeros(Float64, length(np), n_z)
# Θ_0 = [-1000.73282815577369, 1.94909, 7.960764, 0.0001, 0.35536066706454367, -0.10823587535855583, 0.2486100147839871, 0.2248080375561089, 0.6206315818689631]
# Θ_0 = [-762.079,1.774,10.7504, 0.13671037841663608026, 0.79119170457287690823, 0.35703443779432908478, 0.42949855017590554684, 0.01820777905828507501, -0.13282396850696606694]
@everywhere κ = zeros(Float64, n_z)
Θ_0 = [100.06, 2, 10.80851]
lopt = optimize(ll!, Θ_0, NelderMead())
# bopt = bboptimize(ll!; SearchRange = [(-1500.0, 1000.0), (0.001, 5.0), (5.0, 20.0)], Method = :probabilistic_descent)
