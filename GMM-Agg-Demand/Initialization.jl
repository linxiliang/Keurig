# This file defines the appropriate modules and functions

# Module file defining the market class
struct BasicMarket
    Z::Array{Float64, 1} # A vector governing Adoption
    X::Array{Float64, 2} # Variables governing Purchases
    G::Array{Float64, 1} # A vector indicating whether the product is ground
    Price::Array{Float64, 1} # A vector product prices (per unit)
    N::Int64 # Number of cumulative Adopters
    M::Int64 # Number of consumers in the market
    A::Int64 # Adopters of the current period
    sales::Array{Float64, 1}
end

struct Market
    market::BasicMarket
    hshare::Float64
    cshare::Array{Float64, 1}
    Market(market::BasicMarket) = new(market, market.A/(market.M-market.N+market.A),
    market.sales ./ (consumption_rate*market.M))
end

struct FullMarket
	sig::Float64
    market::Market
	meanU::Array{Float64, 1}
    FullMarket(sig::Float64, market::Market) = new(sig, market,
	xifun(sig, market))
end

# Nested Fixed Point Algorithm
function xifun(sig::Float64, m::Market)
    const scaling_param = sqrt(1./pi)
    const nprod = length(m.market.sales)
    meanU = zeros(Float64, nprod)
    err = 1.0
    while (err >= 1e-9)
        UMat = exp.(meanU .+ m.market.Price .* (sqrt(2.) * sig * hermite_nodes'))
        PMat_N = UMat ./ (sum(UMat, 1) .+ 1)
        PMat_M = UMat .* m.market.G ./ (sum(UMat .* m.market.G, 1).+1)
        SVec_N = scaling_param * sum(PMat_N .* hermite_wts', 2)[:,1]
        SVec_M = scaling_param * sum(PMat_M .* hermite_wts', 2)[:,1]
        SVec = m.market.N/(m.market.M) .* SVec_N +
        (m.market.M - m.market.N)/(m.market.M) .* SVec_M
        # Nested Fix Point Algorithm
        meanU_new = meanU + log.(m.cshare) - log.(SVec)
        err = sum((meanU_new - meanU).^2)
        #println("The error is $(err)")
        meanU = meanU_new
    end
    # Compute gradient with respect to
    return meanU[:,1]
end

# Inferior to the above Gauss-Hermite Method.
function rxifun(sig::Float64, m::Market)
    const nprod = length(m.market.sales)
    meanU = zeros(Float64, nprod)
    err = 1.0
    rnodes = randn(3000)
    while (err >= 1e-9)
        UMat = exp.(meanU .+ m.market.Price .* (sig * rnodes'))
        PMat_N = UMat ./ (sum(UMat, 1).+1)
        PMat_M = UMat .* m.market.G ./ (sum(UMat .* m.market.G, 1).+1)
        SVec_N = mean(PMat_N, 2)
        SVec_M = mean(PMat_M, 2)
        SVec = m.market.N/(m.market.M) .* SVec_N +
        (m.market.M - m.market.N)/(m.market.M) .* SVec_M
        # Nested Fix Point Algorithm
        meanU_new = meanU + log.(m.cshare) - log.(SVec)
        err = sum((meanU_new - meanU).^2)
        #println("The error is $(err)")
        meanU = meanU_new
    end
    return meanU[:,1]
end

# Functions related to the DP problem
# Hermite integration of 1-d distribution
function hermiteint(f::Function, μ::Float64, σ::Float64)
 return sqrt(1./pi) * sum(map(f, exp(sqrt(2.)*σ*hermitenodes + μ).- 1.) .* hermitewts);
end

# Hermite integration of multiple 1-d independent distributions
function hermiteint2d(w_tensor::Array{Float64,3}, μ::Array{Float64,1}, σ::Array{Float64,1}, pbar::Real)
  nx = length(μ);
  nd = length(hermitenodes);
  xn = exp(sqrt(2.)*σ*hermitenodes' .+ μ);
  xn[2,:] = xn[2,:] .- 1.0;
  EV = 0;
  for i in 1:nd
    for j in 1:nd
      EV += hermitewts[i] * hermitewts[j] * chebyshev_evaluate(w_tensor, [xn[1, i], pbar, xn[2,j]], order_tensor, range);
    end
  end
  return EV*(sqrt(1./pi))^nx
end

# Bellman iteration with states once the consumer adopts the machine
function W1V(cweights::Array{Float64, 1})
   wappx(x::Real) = chebyshev_evaluate(cweights,[x],delta_order,delta_range)
   function wfun(ν::Real)
       return ν + β * hermiteint(wappx, α0+α1*log(ν+1), σ0)
   end
   return map(wfun, delta_nodes)
end

# Numerical Integration for multiple independent normals.
function hermiteint2d(w_tensor::Array{Float64,3}, μ::Array{Float64,1}, σ::Array{Float64,1}, pbar::Real)
  nx = length(μ);
  nd = length(hermitenodes);
  xn = exp(sqrt(2.)*σ*hermitenodes' .+ μ);
  xn[2,:] = xn[2,:] .- 1.0;
  EV = 0;
  for i in 1:nd
    for j in 1:nd
      EV += hermitewts[i] * hermitewts[j] * chebyshev_evaluate(w_tensor, [xn[1, i], pbar, xn[2,j]], order_tensor, range);
    end
  end
  return EV*(sqrt(1./pi))^nx
end

# Update the treatment effects 
function wupdate!(θ_a::Vector, w_tensor::Array{Float64, 3})
  for h in 1:nobs
    w0_b[h] = chebyshev_evaluate(w_tensor, XMat[h,:], order_tensor, range);
  end
  return 0
end

function ll_fun!(Θ_a::Vector, κ::Vector)
  w1_b[:] = (Θ_a[1] - (Θ_a[2]^2) * XMat[:,1] +  W1Vec)/(Θ_a[3]^2) + ZMat*κ;

  #w1_b[:] = (Θ_a[1] - (Θ_a[2]^2) * XMat[:,1] +  W1Vec)/(Θ_a[3]^2) + ZMat*[0.13671037841663608026, 0.79119170457287690823, 0.35703443779432908478, 0.42949855017590554684, 0.01820777905828507501, -0.13282396850696606694];
  pvec = exp(w1_b)./(exp(w0_b)+exp(w1_b));

  # Compute likelihood for old and new
  ll = -sum(log(pvec).*purch_vec + log(1-pvec).*(1-purch_vec));
  return ll
end

function ll_fun_grad!(Θ_a::Vector, κ::Vector)
  w1_b[:] = (Θ_a[1] - (Θ_a[2]^2) * XMat[:,1] +  W1Vec)/(Θ_a[3]^2) + ZMat*κ;
  pvec = exp(w1_b)./(exp(w0_b)+exp(w1_b));
  return -vec(sum((purch_vec - pvec).*ZMat, 1))
end

llvec = zeros(Float64, length(np))
function ll!(Θ_a::Vector)
  #Θ_a = [Θ_a[1], 2.50438, Θ_a[2]]
  println("The parameter is $(Θ_a)")
  err = 1;
  nx = 0;
  broad_mpi(:(Θ_a = $Θ_a))
  wgrid_new = SharedArray(Float64, n1, n2, n3);
  if isnan(wgrid[1,1,1])
    wgrid[:,:,:] = wgrid_new
  end
  while (err > tol && !isnan(err))
      nx = nx+1;
      w_tensor = chebyshev_weights(wgrid, nodes_1, nodes_2, nodes_3, order_tensor, range);
      broad_mpi(:(w_tensor = $w_tensor))
      @sync begin
        @parallel for (i,j,k) in mgrid
          pbar_n2 =  ω * nodes_1[i] + (1-ω) * nodes_2[j];
          # mu = [ρ0 + ρ1 * log(pbar_n2)+ρ2 * log(nodes_3[k]+1), α0 + α1 * log(nodes_3[k]+1)];
          mu = [ρ0 + ρ1 * log(pbar_n2), α0 + α1 * log(nodes_3[k]+1)];
          EW_v0 = β * hermiteint2d(w_tensor, mu, sigma, pbar_n2)/(Θ_a[3]^2);
          EW_v1 = (Θ_a[1] - (Θ_a[2]^2) * nodes_1[i] + EW1x[k])/(Θ_a[3]^2);
          wgrid_new[i, j, k] = Θ_a[3]^2 * log(exp(EW_v0) + exp(EW_v1))
          # wgrid_new[i, j, k] = Θ_a[3]^2 * log(exp(β*EW_v/(Θ_a[3]^2)) + exp((Θ_a[1] + Θ_a[2] * nodes_1[i] + EW1x[k])/(Θ_a[3]^2)))
       end
      end
      err = sum(abs(wgrid_new-wgrid))
      #println("error is $(err) with nx $(nx)")
      wgrid[:,:,:] = wgrid_new;
  end
  w_tensor = chebyshev_weights(wgrid, nodes_1, nodes_2, nodes_3, order_tensor, range);

 # Directly approximate the expected value of the next period W.
  for (i,j,k) in mgrid
    pbar_n2 =  ω * nodes_1[i] + (1-ω) * nodes_2[j];
    # mu = [ρ0 + ρ1 * log(pbar_n2) + ρ2 * log(nodes_3[k]+1), α0 + α1 * log(nodes_3[k]+1)];
    mu = [ρ0 + ρ1 * log(pbar_n2), α0 + α1 * log(nodes_3[k]+1)];
    ExpectedW[i, j, k] = β * hermiteint2d(w_tensor, mu, sigma, pbar_n2)/(Θ_a[3]^2);
  end

  w_tensor = chebyshev_weights(ExpectedW, nodes_1, nodes_2, nodes_3, order_tensor, range);
  broad_mpi(:(w_tensor = $w_tensor))

  if isnan(err)
    println("The parameter vector is not feasible")
    return 1.0e9
  end

  # Update the W0 value
  @sync begin
      for p in np
          @async @spawnat p eval(:(wupdate!(Θ_a, w_tensor)))
      end
  end

  # Embed the kappa search here!
  function ll_int!(κ::Vector)
    broad_mpi(:(κ[:] = $κ))
    @sync begin
        for p in np
            @async llvec[p-1] = fetch(@spawnat p eval(:(ll_fun!(Θ_a, κ))))
        end
    end
    return sum(llvec)
  end
  function ll_grad!(storage, κ)
    broad_mpi(:(κ[:] = $κ))
    @sync begin
        for p in np
            @async ll_w_grad[p-1, :] = fetch(@spawnat p eval(:(ll_fun_grad!(Θ_a, κ))))
        end
    end
    storage[:] = vec(sum(ll_w_grad, 1))
  end

  llopt = optimize(ll_int!, κ, NelderMead())
  κ[:] = llopt.minimizer
  println("The likelihood is $(llopt.minimum)")
  return llopt.minimum
end


# Settings of a market
Z = [1.0, 145.6];
X = [1.0 0.0 0.0 0.0 0.16; 0.0 1.0 0.0 1.0 0.48; 0.0 0.0 1.0 1.0 0.50];
G = [1.0, 0.0, 0.0];
Price = X[:, end]
N = 200;
M = 200;
A = 10;
sales = [50.0, 10.0, 15.0];
bmkt = BasicMarket(Z, X, G, Price, N, M, A, sales);
mkt = Market(bmkt);
fmkt = FullMarket(2.0, mkt);

# Check whether xifun and rxifun give the same results
err_vec = [sum((xifun(Float64(i), mkt) - rxifun(Float64(i), mkt)).^2) for i in 1:20]


# Check whether xifun and rxifun give the same results
if (maximum(sqrt.(err_vec)./collect(1:20))<0.05)
    println("error checking passed! ")
else
    println("Two functions doesn't match!!!")
end
