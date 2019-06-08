# Functions
(hermitenodes, hermitewts)=gausshermite(15);
function hermiteint(f::Function, μ::Float64, σ::Float64)
  return sqrt(1./pi) * sum(map(f, exp(sqrt(2.)*σ*hermitenodes + μ).- 1.) .* hermitewts);
end

# Numerical Integration for independent normals.
function cheb_eval(w_tensor, x, order_tensor, range)
  x_new = x .* ((range[1,:].>=x) .* (range[2,:].<=x)) .+ range[1,:] .* (range[1,:].<=x) .+  range[2,:] .* (range[2,:].>=x)
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

function W1V(cweights::Array{Float64, 1})
   wappx(x::Real) = cheb_eval(cweights,[x],delta_order,delta_range)
   function wfun(ν::Real)
       return ν + β * hermiteint(wappx, α0+α1*log(ν+1), σ0)
   end
   return map(wfun, delta_nodes)
end

function WApprox(θ1::Vector, θ0::Matrix, H::Matrix, s1::Vector, s0::Matrix, sH::Matrix, wvec::Vector)
     cdist = MvNormal(θ1, H)
     wts0 = pdf(cdist, θ0)
     sdist = MvNormal(s1, sH)
     wts1 = pdf(sdist, s0)
     wts = wts0 .* wts1
     wts = wts ./ sum(wts)
     return (sum(wts .* wvec))
end

function W0V(θ1::Vector, x::Vector, w::Float64)
     return abs(θ1[3]) * log(exp(β*w/abs(θ1[3])) + exp((θ1[1] + θ1[2] * x[1] + wappx(x[3]))/abs(θ1[3])))
end

# Compute the likelihood in each data.
function vupdate!()
   #Update weights and ww_value for old theta
   snew = pdf(snew_dist, SMat)
   wts_old[:] = wts_old - tpdf[1] * spdf[:,1] + tnew * snew;
   ww_old[:] = ww_old - tpdf[1] * wtild[1] * spdf[:,1] + wnew * tnew * snew;

   # Update densities for states
   spdf[:,:] = hcat(spdf[:, 2:end], snew);
   shift!(tpdf);
   push!(tpdf, tnew);
   shift!(wtild);
   push!(wtild, wnew);
   shift!(sdist)
   push!(sdist, snew_dist)

   # Update pdf, weights, and ww_value for new theta
   tpdf1[:] =  pdf(tdist1, thtild);
   wts[:] = spdf * tpdf1;
   ww[:] = spdf * (tpdf1 .* wtild);

   # Update expoential value of adoption for new theta
   # w1_new[:] = (theta1[1] + theta1[2] * XMat[:,1] +  W1Vec)/abs(theta1[3]) + ZMat*kappa1
   w1_new[:] = (theta1[1] + theta1[2] * XMat[:,1] +  W1Vec)/abs(theta1[3])

   # Approximation
   Wa_old = ww_old./wts_old;
   Wa_new = ww./wts;

   # Compute exponential of value of not adopting
   w0_old = β/abs(theta0[3]) .* Wa_old
   w0_new = β/abs(theta1[3]) .* Wa_new

   # Compute the probability of waiting
   delta_old[:] = w1_old - w0_old
   delta_new[:] = w1_new - w0_new
end

# Distributed LL function for logistic regression - OLD parameters
function logit_ll_old(κ::Array{Float64, 1})
  pr = 1.0 ./(1.0 .+ exp.(-(ZMat * κ .+ delta_old)))
  ll = - (purch_vec' * log.(pr) .+ (1.0 .- purch_vec)' * log.(1.0 .- pr))[1,1]
end

ll_old_vec = rand(length(np))
function logit_ll_old_agg(κ::Array{Float64, 1})
  @sync begin
      for p in np
            @async ll_old_vec[p-1] = fetch(@spawnat p logit_ll_old(κ))
      end
  end
  return(sum(ll_old_vec))
end

function logit_grad_old(κ::Array{Float64, 1})
  pr = 1.0 ./(1.0 .+ exp.(-(ZMat * κ .+ delta_old)))
  grad = - (ZMat' * (purch_vec .- pr))[:, 1]
end

ll_old_grad_mat = rand(n_z, length(np))
function logit_grad_old_agg!(grad::Array{Float64,1}, κ::Array{Float64, 1})
  @sync begin
      for p in np
            @async ll_old_grad_mat[:, p-1] = fetch(@spawnat p logit_grad_old(κ))
      end
  end
  grad[:] = sum(ll_old_grad_mat,2)[:,1]
end

# Distributed LL function for logistic regression - NEW parameters
function logit_ll_new(κ::Array{Float64, 1})
  pr = 1.0 ./(1.0 .+ exp.(-(ZMat * κ .+ delta_new)))
  ll = - (purch_vec' * log.(pr) .+ (1.0 .- purch_vec)' * log.(1.0 .- pr))[1,1]
end

ll_new_vec = rand(length(np))
function logit_ll_new_agg(κ::Array{Float64, 1})
  @sync begin
      for p in np
            @async ll_new_vec[p-1] = fetch(@spawnat p logit_ll_new(κ))
      end
  end
  return(sum(ll_new_vec))
end

function logit_grad_new(κ::Array{Float64, 1})
  pr = 1.0 ./(1.0 .+ exp.(-(ZMat * κ .+ delta_new)))
  grad = - (ZMat' * (purch_vec .- pr))[:, 1]
end

ll_new_grad_mat = rand(n_z, length(np))
function logit_grad_new_agg!(grad::Array{Float64,1}, κ::Array{Float64, 1})
  @sync begin
      for p in np
            @async ll_new_grad_mat[:, p-1] = fetch(@spawnat p logit_grad_new(κ))
      end
  end
  grad[:] = sum(ll_new_grad_mat,2)[:,1]
end

# Update only if accepted
function supdate!()
     tpdf[:] = tpdf1;
     w1_old[:] = w1_new;
     wts_old[:] = wts;
     ww_old[:] = ww;
end

# M-H functions to compute alpha
function postd(l1::Real, l0::Real, theta1::Vector, theta0::Vector, bhat::Vector, sigb::Matrix)
    return exp(l1 + log(pdf(MvNormal(bhat, sigb), theta1)) - l0 - log(pdf(MvNormal(bhat, sigb), theta0)))
end

function postd(l1::Real, l0::Real, theta1::Vector, theta0::Vector, kappa1::Vector, kappa0::Vector, bhat::Vector, sigb::Matrix)
    return exp(l1 + log(pdf(MvNormal(bhat, sigb), vcat(theta1, kappa1))) - l0 - log(pdf(MvNormal(bhat, sigb), vcat(theta0, kappa0))))
end


# Fast Logistic regression
function logit_ll(κ::Array{Float64, 1}, delta::Array{Float64, 1}, X::Array{Float64, 2}, Y::Array{Float64, 1})
  pr = 1.0 ./(1.0 .+ exp.(-(X * κ .+ delta)))
  ll = - (Y' * log.(pr) .+ (1.0 .- Y)' * log.(1.0 .- pr))[1,1]
end

function logit_grad!(grad::Array{Float64,1}, κ::Array{Float64, 1}, delta::Array{Float64, 1}, X::Array{Float64, 2}, Y::Array{Float64, 1})
  pr = 1.0 ./(1.0 .+ exp.(-(X * κ .+ delta)))
  grad[:] = - (X' * (Y .- pr))[:, 1]
end

function logit_hessian!(hess::Array{Float64,2}, κ::Array{Float64, 1}, delta::Array{Float64, 1}, X::Array{Float64, 2}, Y::Array{Float64, 1})
  pr = 1.0 ./(1.0 .+ exp.(-(X * κ .+ delta)))
  N = length(Y)
  for i in 1:N
    hess[:,:] += X[i, :] * X[i, :]' * (pr[i] * (1.0 .- pr[i]))
  end
end
