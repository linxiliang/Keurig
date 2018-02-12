# GMM Estimation of the Aggregate data model
# Xiliang Lin
# Feb, 2018

# Setup parallel computing environment
remote_machine = false;
if remote_machine
  addprocs(20, restrict=false)
  machines = [("bushgcn02", 20), ("bushgcn04", 20), ("bushgcn05", 20), ("bushgcn06", 20)]
  # machines = [("bushgcn33", 28)]
  # machines = [("bushgcn11", 12), ("bushgcn12", 12)]
  addprocs(machines; tunnel=true)
else
  addprocs(4; restrict=false)
end
np = workers()
remotecall_fetch(rand, 2, 20) # Test worker

function broad_mpi(expr::Expr)
  for p in np
      @spawnat p eval(expr)
  end
end
broad_mpi(:(np = $np));

# Set Working directory
# @everywhere cd("$(homedir())/Keurig");
@everywhere cd("/Volumes/SanDisk/Keurig");

# Load necessary library
using FastGaussQuadrature
include("Scripts/GMM-Agg-Demand/Initialization.jl")
broad_mpi(:(using FastGaussQuadrature))
@everywhere global (hermite_nodes, hermite_wts) = gausshermite(21);
@everywhere const consumption_rate = 3;
broad_mpi(:(include("Scripts/GMM-Agg-Demand/Initialization.jl")))

# Settings of a market
Z = [1.0, 145.6];
X = [1.0 0.0 0.0 0.0 0.16; 0.0 1.0 0.0 1.0 0.48; 0.0 0.0 1.0 1.0 0.50];
G = [1.0, 0.0, 0.0];
N = 100;
M = 200;
A = 10;
sales = [50.0, 10.0, 15.0];
bmkt = BasicMarket(Z, X, G, N, M, A, sales);
mkt = Market(bmkt);
xifun(2.0, mkt)
fmkt = FullMarket(2.0, mkt)

# Check whether xifun and rxifun give the same results
err_vec = [sum((xifun(Float64(i), mkt) - rxifun(Float64(i), mkt)).^2) for i in 1:20]

# Outer Constructor to construct types
function Derived(Z, X, x)
end
# market(Z::Array{Float64, 2}, X::Array{Float64, 2}, N::Int64, M::Int64, A::Int64,
# sales::Array{Float64, 1})

# Gauss Hermite Nodes
#sum((sqrt(2.)*hermite_nodes).^2 .* hermite_wts)*(sqrt(1./pi))^1
#exp(sqrt(2.)*σ*hermitenodes' .+ μ)
#exp(sqrt(2.)*σ*hermitenodes' .+ μ)

# Test whether Atom git works
finalize(workers())
