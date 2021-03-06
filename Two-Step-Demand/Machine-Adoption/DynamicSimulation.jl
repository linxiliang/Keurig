module DSimulation

# Dependencies
import Distributions, ApproxFun, FastGaussQuadrature
const Distr = Distributions
const AF = ApproxFun
const FGQ = FastGaussQuadrature

# Export certain functions
export user_profile, foo

struct user_profile
  periods::Int64
  state::Array{Float64, 2}
  action::Array{Float64, 1}
  payoffs::Array{Float64, 1}
  # Payoffs may be unobserved, and thus constructed
end

mutable struct user_full_profile
  periods::Int64
  beta::Float64
  theta::Array{Float64, 1}
  state::Array{Float64, 2}
  action::Array{Float64, 1}
  payoffs::Array{Float64, 1}
  noise::Array{Float64, 1}
end

struct user_set
  users::Array{Int64, 1}
end

struct state_distribution
  mu::Array{Float64, 1}
  sigma::Array{Float64, 1}
  # Put in check for dimensionality
end

struct action_space
  actions::Array{Int64, 1}
end

struct cheby_set_init
  domain::AF.Interval
  n::Int64
end

struct cheby_set
  domain::AF.Chebyshev
  points::Array{Float64}
end

struct cheby_fun
  domain::AF.Chebyshev
  points::Array{Float64}
  coef::Array{Float64}
end

# Methods
function ErrEval(V1::Function, V2::Function, grid::Array{Array{Float64, 1}, 1})
  sumerr2 = sum((map(V1, grid) .- map(V2, grid)).^2)
  return(sumerr2)
end

function ErrEval(V1::Function, V1n::Function, grid::Array{Float64, 1})
  sumerr2 = sum((map(V1, grid) .- map(V1n, grid)).^2)
  return(sumerr2)
end

function ErrEval(V1::Function, V1n::Function, grid)
  sumerr2 = sum((map(V1, grid) .- map(V1n, grid)).^2)
  return(sumerr2)
end

# Get approximation nodes
function HermiteIntialize(n::Int64)
  (nodes, wts) = FGQ.gausshermite(15);
  hermite_set = (nodes = nodes, wts = wts)
  return(hermite_set)
end

function DeltaTransit(delta::Float64, param::NamedTuple)
  mu = param.α0 + param.α1 * log(delta+1)
  return(mu = mu, sigma=param.sigma_α)
end

function HermiteIntegrate(V1::Function, delta::Float64, param::NamedTuple, hermite_set::NamedTuple)
  (mu, sigma) = DeltaTransit(delta, param)
  _grid = exp.(sqrt(2.) * sigma * hermite_set.nodes .+ mu) .- 1.
  _val = map(V1, _grid)
  _est = sqrt(1. / pi) * sum(_val .* hermite_set.wts)
  return(_est)
end

function MCIntegrate(V1::Function, delta::Float64, param::NamedTuple, n::Int64)
  (mu, sigma) = DeltaTransit(delta, param)
  _grid_distr = Normal(mu, sigma)
  _grid = rand(_grid_distr, n)
  _grid = exp.(_grid) .- 1.0
  _val = mapreduce(V1, +, _grid)
  est = _val/n
  return(est)
end

function DynamicEquation(V1::Function, param::NamedTuple, cset::cheby_set, hermite_set::NamedTuple)
  future_V1(delta::Float64) = HermiteIntegrate(V1, delta, param, hermite_set)
  V1_iter(delta::Float64) = delta + param.discount * future_V1(delta)
  new_coef = AF.transform(cset.domain, map(V1_iter, cset.points))
  return(cheby_fun(cset.domain, cset.points, new_coef))
end

function DynamicEquation(cfun::cheby_fun, param::NamedTuple, hermite_set::NamedTuple)
  V1 = AF.Fun(cfun.domain, cfun.coef)
  V1_full(delta::Float64) = AF.extrapolate(V1, delta)
  future_V1(delta::Float64) = HermiteIntegrate(V1, delta, param, hermite_set)
  V1_iter(delta::Float64) = delta + param.discount * future_V1(delta)
  new_coef = AF.transform(cfun.domain, map(V1_iter, cfun.points))
  return(cheby_fun(cfun.domain, cfun.points, new_coef))
end

function DynamicIteration(V1::Function, param::NamedTuple, cset::cheby_set,
    hermite_set::NamedTuple, tol::Float64)
    err = 1.0
    V1_cheby = DynamicEquation(V1, param, cset, hermite_set)
    while (err > tol)
        V1n = DynamicEquation(V1_cheby, param, hermite_set)
        err = ErrEval(AF.Fun(V1_cheby.domain, V1_cheby.coef),
                      AF.Fun(V1n.domain, V1n.coef), grid)
        # println("Current error is $(err)")
        V1_cheby = V1n
    end
    V1_new = AF.Fun(V1_cheby.domain, V1_cheby.coef)
    V1_full(delta::Float64) = AF.extrapolate(V1_new, delta)
    return(V1_full)
end

# Solve for a fixed point
grid = range(1, 5, step = .01)
cset = cheby_set_init(AF.Interval(1,5), 20)
cset = cheby_set(AF.Chebyshev(cset.domain),
                 AF.points(AF.Chebyshev(cset.domain), cset.n))
hermite_set = HermiteIntialize(15)

# Value function iteration for adoption
tol = 1e-10
err = 1
V1_init(delta::Float64) = 0.0
V1 = DynamicIteration(V1_init, param, cset, hermite_set, tol)

# Coding the state transitions! 
# Currently, state transition is not a function of a, but in general should be
function StateTransit(s::Array{Float64, 1}, param::NamedTuple)
  pbar_n2 =  param.ω * s[1] + (1 - param.ω) * s[2];
  mu = [param.ρ0 + param.ρ1 * log(pbar_n2), param.α0 + param.α1 * log(s[3]+1)]
  s_next_distr = state_distribution(mu, param.sigma_α)
  return(s_next_distr)
end

param = (discount = 0.98, α0 = 0.01, α1 = 0.95, sigma_α = 0.10, ω = 0.2816747,
         ρ0 = 7.1280045, ρ1 = 0.9504942, sigma_p = 6.593)

function U(a::Int64, s::Array{Float64,1}, theta::Array{Float64,1})
  if (a = 0)
    return(0.0)
  elseif (a=1)
    return(theta.tau + theta.beta * s[1] + s[3])
  end
end

# Adoption value
function W1V(cweights::Array{Float64, 1})
   wappx(x::Real) = cheb_eval(cweights, [x], delta_order, delta_range)
   function wfun(ν::Real)
       return ν + β * hermiteint(wappx, α0+α1*log(ν+1), σ0)
   end
   return map(wfun, delta_nodes)
end

function W0V(theta::Array{Float64,1}, cweights::Array{Float64, 3})
  (n1, n2, n3) = size(wgrid)
  wgrid_new = zeros(n1, n2, n3)
  for i in 1:n1,  j in 1:n2, k in 1:n3
    pbar_n2 =  ω * nodes_1[i] + (1-ω) * nodes_2[j];
    mu = [ρ0 + ρ1 * log(pbar_n2), α0 + α1 * log(nodes_3[k]+1)];
    EW_v0 = β * hermiteint2d(cweights, mu, sigma, pbar_n2);
    EW_v1 = theta[1] + theta[2] * nodes_1[i] + wappx(nodes_3[k]);
    wgrid_new[i, j, k] = abs(theta[3]) * log(exp(EW_v0/abs(theta[3])) + exp(EW_v1/abs(theta[3])))
  end
  return wgrid_new
end


struct dynamic_system
  discount::Float64
  param::NamedTuple
  U::Function # Period period utility, function of action and state
  StateTransit::Function # Given state, transition to next state distribution
end



function adoption_value_iterate(V1, param)
  s_next_distr(s) = ds.StateTransit(s, param)
  V1_new(delta) = delta + integrate(V1, delta)
end
actions::Array{Int64, 1} # Discrete actions
state::Array{Int64, 1}

function Bellman(W::Function, ds::dynamic_system)
  # Get the distribution of next period's state
  s_next_distr(s) = ds.StateTransit(s, param)
  s::Array{Float64, 1}
  # Bellman Iteration
  # Waiting Value
  expected_waiting_value(s) = integrate(W, s, param)
  # Adoption Value
  adoption_value(s) =  adopt_value(s, param)

  log(exp(discount*W(s)) +)
  new_W(s) = V(s, a) + discount * int(V(x), state_transition)

end



function ErrEval(V1::Function, V2::Function, grid::Array{Array{Float64, 1}, 1})
  sumerr2 = sum((map(V1, grid) .- map(V2, grid)).^2)
  return(sumerr2)
en

function ValueFuntionIterate(V1, u, state, discount, tol = 1e-10)
  # Value function iteration until error is less than tolerance
  loop = 0
  while (err2 > tol  & loop <= 10000)
    # perform bellman iteration
    V2 = bellman(u, V1, state, discount)

    # Compute the error
    err2 = ErrEval(V1, V2, grid)

    loop += 1
    V1 = V2
  end
  loop < 10000 || println("Took more than 10000 loops")
  return(V1)
end



bar(x) = 2x
foo(a::MyType) = bar(a.x) + 1


end
(hermitenodes, hermitewts)=gausshermite(15);
hermite
function hermiteint(f::Function, μ::Float64, σ::Float64)
  return sqrt(1./pi) * sum(map(f, exp(sqrt(2.)*σ*hermitenodes + μ).- 1.) .* hermitewts);
end

# Numerical Integration for independent normals.
function cheb_eval(w_tensor, x, order_tensor, range)
  x_new = x .* ((range[1,:].>=x) .* (range[2,:].<=x)) .+ range[1,:] .* (range[1,:].<=x) .+  range[2,:] .* (range[2,:].>=x)
  w = chebyshev_evaluate(w_tensor, x_new, order_tensor, range);
  return w
end

# Two dimension integration
function hermiteint2d(w_tensor::Array{Float64,3}, μ::Array{Float64,1}, σ::Array{Float64,1}, pbar::Real)
  nx = length(μ);
  nd = length(hermitenodes);
  xn = exp(sqrt(2.)*σ*hermitenodes' .+ μ);
  xn[2,:] = xn[2,:] .- 1.0;
  EV = 0;
  for i in 1:nd
    for j in 1:nd
      EV += hermitewts[i] * hermitewts[j] * cheb_eval(w_tensor, [xn[1, i], pbar, xn[2,j]], order_tensor, range);
    end
  end
  return EV*(sqrt(1./pi))^nx
end

# Monte Carlo Integration
function  mcint2d(w_tensor::Array{Float64,3}, μ::Array{Float64,1}, σ::Array{Float64,1}, pbar::Real, N::Int64)
  EV = 0
  x_distr = MvNormal(μ, diagm(σ.^2))
  for i in 1:N
    x_i = rand(x_distr)
    x_i = exp.(x_i)
    x_i[2] = x_i[2] - 1.0
    EV += cheb_eval(w_tensor, [x_i[1], pbar, x_i[2]], order_tensor, range);
  end
  return EV/N
end


function W1V(cweights::Array{Float64, 1})
   wappx(x::Real) = cheb_eval(cweights, [x], delta_order, delta_range)
   function wfun(ν::Real)
       return ν + β * hermiteint(wappx, α0+α1*log(ν+1), σ0)
   end
   return map(wfun, delta_nodes)
end

function W0V(theta::Array{Float64,1}, cweights::Array{Float64, 3})
  (n1, n2, n3) = size(wgrid)
  wgrid_new = zeros(n1, n2, n3)
  for i in 1:n1,  j in 1:n2, k in 1:n3
    pbar_n2 =  ω * nodes_1[i] + (1-ω) * nodes_2[j];
    mu = [ρ0 + ρ1 * log(pbar_n2), α0 + α1 * log(nodes_3[k]+1)];
    EW_v0 = β * hermiteint2d(cweights, mu, sigma, pbar_n2);
    EW_v1 = theta[1] + theta[2] * nodes_1[i] + wappx(nodes_3[k]);
    wgrid_new[i, j, k] = abs(theta[3]) * log(exp(EW_v0/abs(theta[3])) + exp(EW_v1/abs(theta[3])))
  end
  return wgrid_new
end

function simProb(theta::Array{Float64,1}, s::Array{Float64,1}, cweights::Array{Float64, 3},
  kappa::Array{Float64,1}, z::Array{Float64,1})
  pbar_n2 =  ω * s[1] + (1-ω) * s[2];
  mu = [ρ0 + ρ1 * log(pbar_n2), α0 + α1 * log(s[3]+1)];
  EW_v0 = β * hermiteint2d(cweights, mu, sigma, pbar_n2);
  EW_v1 = theta[1] + theta[2] * s[1] + wappx(s[3]);
  prob = exp(EW_v1/theta[3] + z' * kappa)/(exp(EW_v1/theta[3] + z' * kappa) + exp(EW_v0/theta[3]))
  return prob
end

function W0Cheby(theta::Array{Float64,1}, s::Array{Float64,1}, cweights::Array{Float64, 3})
  pbar_n2 =  ω * s[1] + (1-ω) * s[2];
  println(pbar_n2)
  mu = [ρ0 + ρ1 * log(pbar_n2), α0 + α1 * log(s[3]+1)];
  println(mu)
  EW0 = hermiteint2d(cweights, mu, sigma, pbar_n2);
  EW0_temp = mcint2d(cweights, mu, sigma, pbar_n2, 100000);
  return (EW0, EW0_temp)
end

function simProb(theta::Array{Float64,1}, s::Array{Float64,1}, cweights::Array{Float64, 3},
  kappa::Array{Float64,1}, z::Array{Float64,1})
  pbar_n2 =  ω * s[1] + (1-ω) * s[2];
  mu = [ρ0 + ρ1 * log(pbar_n2), α0 + α1 * log(s[3]+1)];
  EW_v0 = β * hermiteint2d(cweights, mu, sigma, pbar_n2);
  EW_v1 = theta[1] + theta[2] * s[1] + wappx(s[3]);
  prob = exp(EW_v1/theta[3] + z' * kappa)/(exp(EW_v1/theta[3] + z' * kappa) + exp(EW_v0/theta[3]))
  return prob
end

function WApprox(θ1::Vector, θ0::Matrix, H::Matrix, s1::Vector, s0::Matrix, sH::Matrix, wvec::Vector)
  cdist = MvNormal(θ1, H)
  wts0 = pdf(cdist, θ0)
  sdist = MvNormal(s1, sH)
  wts1 = pdf(sdist, s0)
  wts = wts0 .* wts1
  wts = wts ./ sum(wts)
  return (sum(wts' * wvec))
end

function WApprox(θ1::Matrix, θ0::Matrix, H::Matrix, s1::Matrix, s0::Matrix, sH::Matrix, wvec::Vector)
  n = size(θ1)[2]
  approx_vec = zeros(n)
  for i = 1:n
    approx_vec[i] = WApprox(θ1[:,i], θ0, H, s1[:,i], s0, sH, wvec)
  end
  return approx_vec
end

function W0V(θ1::Vector, x::Vector, w::Float64)
     return abs(θ1[3]) * log(exp(β*w/abs(θ1[3])) + exp((θ1[1] + θ1[2] * x[1] + wappx(x[3]))/abs(θ1[3])))
end

# Update the density distributions
function WoldStatesUpdate!(theta1::Array{Float64, 1}, tx_i::Array{Float64, 1},
  w_i::Float64, theta0::Array{Float64, 1})
  # Compute old pdf
  tdist_old = MvNormal(thtild[:, 1], H)
  tpdf_old = pdf(tdist_old, theta0)
  sold_dist = MvNormal(txtild[:, 1], sH)
  spdf_old = pdf(sold_dist, SMat)

  # Compute new pdf
  tdist_new = MvNormal(theta1, H)
  tpdf_new = pdf(tdist_new, theta0)
  snew_dist = MvNormal(tx_i, sH)
  spdf_new = pdf(snew_dist, SMat)

  # Update old weights
  wts_old[:] = wts_old - tpdf_old * spdf_old + tpdf_new * spdf_new;
  ww_old[:] = ww_old - tpdf_old * wtild[1] * spdf_old + w_i * tpdf_new * spdf_new;
end

function ApproxStateUpdate!(theta1::Array{Float64, 1}, tx_i::Array{Float64, 1}, w_i::Float64)
  # Update the historical storage of approximation parameters
  thtild[:, 1:(end-1)] = thtild[:, 2:end]
  thtild[:, end] = theta1
  txtild[:, 1:(end-1)] = txtild[:, 2:end]
  txtild[:, end] = tx_i
  wtild[1:(end-1)] = wtild[2:end]
  wtild[end] = w_i
end

# Given theta, update wts and ww for all data
function WApproxAll!(theta::Array{Float64, 1}, wts::Array{Float64, 1},
                     ww::Array{Float64, 1})
  tdist = MvNormal(theta, H)
  tpdf =  pdf(tdist, thtild)
  wts[:] = 0.0
  ww[:] = 0.0
  for i in 1:N_0
    wts_i = tpdf[i] .* pdf(MvNormal(txtild[:,i], sH), SMat)
    wts[:] += wts_i
    ww[:] += wtild[i] .* wts_i
  end
end

# Update Deltas
function UpdateStateDeltas!(theta1::Array{Float64, 1}, tx_i::Array{Float64, 1},
  w_i::Float64, theta0::Array{Float64, 1})

  # Update new w0s
  WApproxAll!(theta1, wts_new, ww_new)

  # Compute new expected value of waiting
  Wa_old = ww_old./wts_old
  Wa_new = ww_new./wts_new
  w0_old = β/abs(theta0[3]) .* Wa_old
  w0_new = β/abs(theta1[3]) .* Wa_new

  # Compute the value of adoption for new theta
  w1_new[:] = (theta1[1] + theta1[2] * XMat[:,1] +  W1Vec)/abs(theta1[3])

  # Compute Updated Deltas
  delta_old[:] = w1_old - w0_old
  delta_new[:] = w1_new - w0_new
end

# Update only if accepted
function supdate!()
     w1_old[:] = w1_new;
     wts_old[:] = wts_new;
     ww_old[:] = ww_new;
end


param = (discount = 0.98, α0 = 0.01, α1 = 0.95, sigma_α = 0.10, ω = 0.2816747,
         ρ0 = 7.1280045, ρ1 = 0.9504942, sigma_p = 6.593)
f(x) = exp(x)
fhat = AF.Fun(cset.domain, AF.transform(cset.domain, map(f, cset.points)))
fhat_full(x) = AF.extrapolate(fhat, x)


@everywhere ω = 0.2816747
# Price: price' = ρ0 + ρ1⋅price + ɛ, ɛ~N(0,σ1^2)
@everywhere ρ0 = 7.1280045
@everywhere ρ1 = 0.9504942
@everywhere σ1 = 6.593
